module Pipeline.Plan exposing (..)

import Data.Either as Either exposing (Either(..))
import Data.FilePath as FilePath exposing (FilePath)
import Data.HashDict as HashDict exposing (HashDict)
import Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Elm.Make.BuildData as BuildData exposing (BuildData)
import Elm.Make.BuildGraph as BuildGraph exposing (BuildGraph)
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Elm.Make.Config as BM
import Elm.Make.Error as BM
import Elm.Make.Location as Location exposing (Location)
import Elm.Make.ProjectData as ProjectData exposing (ProjectData)
import Elm.Make.ProjectGraph as ProjectGraph exposing (ProjectGraph)
import Extra.Graph as Graph
import Extra.Task as Task
import FileStorage as FileStorage
import Graph exposing (Graph)
import Path as Path
import Task exposing (Task)
import Time exposing (Time)


type alias EnhancedGraph =
    HashDict CanonicalModule (ProjectData ( Location, Maybe ( FilePath, Time ) ))


type alias InterfacedGraph =
    HashDict CanonicalModule (ProjectData (Either Location Interface))


planBuild : BM.Config -> ProjectGraph Location -> Task BM.Error BuildGraph
planBuild config { projectData, projectNatives } =
    projectData
        |> HashDict.toList
        |> List.map (\( k, v ) -> enhanceData config.artifactDirectory k v |> Task.map ((,) k))
        |> Task.sequence
        |> Task.map HashDict.fromList
        |> Task.andThen loadCachedInterfaces
        |> Task.map toBuildGraph


enhanceData :
    FilePath
    -> CanonicalModule
    -> ProjectData Location
    -> Task BM.Error (ProjectData ( Location, Maybe ( FilePath, Time ) ))
enhanceData artifactRoot moduleId { projectLocation, projectDependencies } =
    if isMain moduleId then
        Task.succeed <| ProjectData ( projectLocation, Nothing ) projectDependencies
    else
        let
            interfacePath =
                Path.toInterface artifactRoot moduleId

            sourcePath =
                Path.toSource projectLocation
        in
        getFreshInterfaceInfo sourcePath interfacePath
            |> Task.map (\interfaceInfo -> ProjectData ( projectLocation, interfaceInfo ) projectDependencies)


isMain : CanonicalModule -> Bool
isMain { name } =
    case name of
        [ "Main" ] ->
            True

        _ ->
            False


getFreshInterfaceInfo : FilePath -> FilePath -> Task BM.Error (Maybe ( FilePath, Time ))
getFreshInterfaceInfo sourcePath interfacePath =
    FileStorage.exists interfacePath
        |> Task.mapError (\_ -> BM.CorruptedArtifact interfacePath)
        |> Task.andThen
            (\exists ->
                if not exists then
                    Task.succeed Nothing
                else
                    Task.map2 (,)
                        (FileStorage.modified sourcePath
                            |> Task.mapError (\_ -> BM.CorruptedArtifact sourcePath)
                        )
                        (FileStorage.modified interfacePath
                            |> Task.mapError (\_ -> BM.CorruptedArtifact interfacePath)
                        )
                        |> Task.map
                            (\( maybeSourceTime, maybeInterfaceTime ) ->
                                case ( maybeSourceTime, maybeInterfaceTime ) of
                                    ( Just sourceTime, Just interfaceTime ) ->
                                        if Debug.log "diff" (sourceTime <= interfaceTime) then
                                            Just ( interfacePath, interfaceTime )
                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
            )


loadCachedInterfaces : EnhancedGraph -> Task BM.Error InterfacedGraph
loadCachedInterfaces summary =
    topologicalSort (HashDict.map (\k v -> v.projectDependencies) summary)
        |> Task.andThen
            (\sortedNames ->
                Task.foldl (updateFromCache summary) HashDict.empty sortedNames
            )


updateFromCache :
    EnhancedGraph
    -> CanonicalModule
    -> InterfacedGraph
    -> Task BM.Error InterfacedGraph
updateFromCache enhancedGraph moduleName interfacedGraph =
    let
        { projectLocation, projectDependencies } =
            HashDict.getUnsafe moduleName enhancedGraph

        ( location, maybeInterfaceInfo ) =
            projectLocation

        getTrueLocation =
            case maybeInterfaceInfo of
                Nothing ->
                    Task.succeed <| Left location

                Just ( interfacePath, time ) ->
                    if List.all (isCacheValid time enhancedGraph) projectDependencies then
                        Interface.read
                            (\_ -> BM.CorruptedArtifact interfacePath)
                            interfacePath
                            |> Task.map Right
                    else
                        Task.succeed <| Left location
    in
    getTrueLocation
        |> Task.map
            (\trueLocation ->
                HashDict.insert moduleName (ProjectData trueLocation projectDependencies) interfacedGraph
            )


isCacheValid : Time -> EnhancedGraph -> CanonicalModule -> Bool
isCacheValid time enhancedGraph possiblyNativeName =
    case filterNativeDeps possiblyNativeName of
        Nothing ->
            True

        Just name ->
            if CanonicalModule.isElmLang name then
                True
            else
                case .projectLocation (HashDict.getUnsafe name enhancedGraph) of
                    ( _, Just ( _, depTime ) ) ->
                        depTime <= time

                    _ ->
                        False


toBuildGraph : InterfacedGraph -> BuildGraph
toBuildGraph summary =
    let
        ( locations, interfaces ) =
            HashDict.mapEither divide summary

        divide { projectLocation, projectDependencies } =
            case projectLocation of
                Left location ->
                    Left (ProjectData location projectDependencies)

                Right interface ->
                    Right interface
    in
    { blockedModules = HashDict.map (\k v -> toBuildData interfaces v) locations
    , completedInterfaces = interfaces
    }


toBuildData :
    HashDict CanonicalModule Interface
    -> ProjectData Location
    -> BuildData
toBuildData interfaces { projectLocation, projectDependencies } =
    let
        blocking =
            List.filterMap filterDeps projectDependencies

        filterDeps deps =
            filterNativeDeps deps
                |> Maybe.andThen (filterCachedDeps interfaces)
    in
    BuildData blocking projectLocation


filterCachedDeps :
    HashDict CanonicalModule Interface
    -> CanonicalModule
    -> Maybe CanonicalModule
filterCachedDeps interfaces name =
    case HashDict.get name interfaces of
        Just interface ->
            Nothing

        Nothing ->
            Just name


filterNativeDeps : CanonicalModule -> Maybe CanonicalModule
filterNativeDeps name =
    case name.name of
        "Native" :: _ ->
            Nothing

        _ ->
            Just name


topologicalSort : HashDict CanonicalModule (List CanonicalModule) -> Task BM.Error (List CanonicalModule)
topologicalSort dependencies =
    dependencies
        |> HashDict.toList
        |> Graph.fromEdges CanonicalModule.compare
        |> Graph.topologicalSort
        |> Result.map (List.map (.node >> .label))
        |> Result.mapError (List.head >> Maybe.map Graph.nodes >> Maybe.withDefault [] >> List.map .label >> BM.Cycle)
        |> Task.fromResult
