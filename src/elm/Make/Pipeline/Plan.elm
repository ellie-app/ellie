module Make.Pipeline.Plan exposing (..)

import EveryDict exposing (EveryDict)
import Task exposing (Task)
import Time exposing (Time)
import Graph exposing (Graph)
import Data.FilePath as FilePath exposing (FilePath)
import Data.Either as Either exposing (Either(..))
import Data.Extra.Task as Task
import Data.Extra.Graph as Graph
import Data.Extra.EveryDict as EveryDict
import Data.Elm.Make.Config as BM
import Data.Elm.Make.Error as BM
import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Data.Elm.Make.ProjectGraph as ProjectGraph exposing (ProjectGraph)
import Data.Elm.Make.BuildGraph as BuildGraph exposing (BuildGraph)
import Data.Elm.Make.ProjectData as ProjectData exposing (ProjectData)
import Data.Elm.Make.Location as Location exposing (Location)
import Data.Elm.Make.BuildData as BuildData exposing (BuildData)
import Data.Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Make.Path as Path
import Make.FileStorage as FileStorage


type alias EnhancedGraph =
    EveryDict CanonicalModule (ProjectData ( Location, Maybe ( FilePath, Time ) ))


type alias InterfacedGraph =
    EveryDict CanonicalModule (ProjectData (Either Location Interface))


planBuild : BM.Config -> ProjectGraph Location -> Task BM.Error BuildGraph
planBuild config { projectData, projectNatives } =
    projectData
        |> EveryDict.toList
        |> List.map (\( k, v ) -> enhanceData config.artifactDirectory k v |> Task.map ((,) k))
        |> Task.sequence
        |> Task.map EveryDict.fromList
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
                                        if sourceTime <= interfaceTime then
                                            Just ( interfacePath, interfaceTime )
                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
            )


loadCachedInterfaces : EnhancedGraph -> Task BM.Error InterfacedGraph
loadCachedInterfaces summary =
    topologicalSort (EveryDict.map (\k v -> v.projectDependencies) summary)
        |> Task.andThen
            (\sortedNames ->
                Task.foldl (updateFromCache summary) EveryDict.empty sortedNames
            )


updateFromCache :
    EnhancedGraph
    -> CanonicalModule
    -> InterfacedGraph
    -> Task BM.Error InterfacedGraph
updateFromCache enhancedGraph moduleName interfacedGraph =
    let
        { projectLocation, projectDependencies } =
            EveryDict.getUnsafe moduleName enhancedGraph

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
                    EveryDict.insert moduleName (ProjectData trueLocation projectDependencies) interfacedGraph
                )


isCacheValid : Time -> EnhancedGraph -> CanonicalModule -> Bool
isCacheValid time enhancedGraph possiblyNativeName =
    case filterNativeDeps possiblyNativeName of
        Nothing ->
            True

        Just name ->
            case .projectLocation (EveryDict.getUnsafe name enhancedGraph) of
                ( _, Just ( _, depTime ) ) ->
                    depTime <= time

                _ ->
                    False


toBuildGraph : InterfacedGraph -> BuildGraph
toBuildGraph summary =
    let
        ( locations, interfaces ) =
            EveryDict.mapEither divide summary

        divide { projectLocation, projectDependencies } =
            case projectLocation of
                Left location ->
                    Left (ProjectData location projectDependencies)

                Right interface ->
                    Right interface
    in
        { blockedModules = EveryDict.map (\k v -> toBuildData interfaces v) locations
        , completedInterfaces = interfaces
        }


toBuildData :
    EveryDict CanonicalModule Interface
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
    EveryDict CanonicalModule Interface
    -> CanonicalModule
    -> Maybe CanonicalModule
filterCachedDeps interfaces name =
    case EveryDict.get name interfaces of
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


topologicalSort : EveryDict CanonicalModule (List CanonicalModule) -> Task BM.Error (List CanonicalModule)
topologicalSort dependencies =
    dependencies
        |> EveryDict.toList
        |> Graph.fromEdges (CanonicalModule.compare)
        |> Graph.topologicalSort
        |> Result.map (List.map (.node >> .label))
        |> Result.mapError (List.head >> Maybe.map Graph.nodes >> Maybe.withDefault [] >> List.map .label >> BM.Cycle)
        |> Task.fromResult
