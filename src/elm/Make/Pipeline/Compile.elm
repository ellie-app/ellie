module Make.Pipeline.Compile exposing (..)

import Task exposing (Task)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import EveryDict exposing (EveryDict)
import Data.EverySet as EverySet exposing (EverySet)
import Data.Either as Either exposing (Either(..))
import Data.Extra.EveryDict as EveryDict
import Data.Extra.List as List
import Data.Extra.Task as Task
import Data.FilePath as FilePath exposing (FilePath)
import Make.FileStorage as FileStorage
import Make.Elm.Compiler as Compiler
import Data.Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Data.Elm.Make.Error as BM
import Data.Elm.Make.Location as Location exposing (Location)
import Data.Elm.Make.BuildGraph as BuildGraph exposing (BuildGraph)
import Data.Elm.Make.BuildData as BuildData exposing (BuildData)
import Make.Path as Path
import Data.Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)


type alias Model =
    { numTasks : Int
    , dependencies : EveryDict CanonicalModule (List CanonicalModule)
    , reverseDependencies : EveryDict CanonicalModule (List CanonicalModule)
    , cachePath : FilePath
    , exposedModules : EverySet CanonicalModule
    , modulesForGeneration : EverySet CanonicalModule
    , blockedModules : EveryDict CanonicalModule BuildData
    , completedInterfaces : EveryDict CanonicalModule Interface
    , readyList : List ( CanonicalModule, Location )
    , runningList : List CanonicalModule
    , completedTasks : Int
    }



-- HELPERS for ENV and STATE


init :
    FilePath
    -> EverySet CanonicalModule
    -> List CanonicalModule
    -> EveryDict CanonicalModule (List CanonicalModule)
    -> BuildGraph
    -> Model
init cachePath exposedModules modulesForGeneration dependencies buildGraph =
    let
        categorize name buildData =
            case buildData.blocking of
                [] ->
                    Left ( name, buildData.location )

                _ ->
                    Right buildData

        ( readyModules, blockedModules ) =
            EveryDict.mapEitherWithKey categorize buildGraph.blockedModules

        readyList =
            EveryDict.values readyModules
    in
        { numTasks = EveryDict.size buildGraph.blockedModules
        , dependencies = dependencies
        , reverseDependencies = reverseGraph dependencies
        , cachePath = cachePath
        , exposedModules = exposedModules
        , modulesForGeneration = EverySet.fromList modulesForGeneration
        , blockedModules = blockedModules
        , completedInterfaces = buildGraph.completedInterfaces
        , readyList = readyList
        , runningList = []
        , completedTasks = 0
        }


reverseGraph : EveryDict CanonicalModule (List CanonicalModule) -> EveryDict CanonicalModule (List CanonicalModule)
reverseGraph graph =
    let
        insertDependency name dep reversedGraph =
            EveryDict.insertWith (flip (++)) dep [ name ] reversedGraph

        flipEdges name dependencies reversedGraph =
            List.foldr (insertDependency name) reversedGraph dependencies
    in
        EveryDict.foldr flipEdges EveryDict.empty graph


registerSuccess :
    CanonicalModule
    -> Interface
    -> Model
    -> Model
registerSuccess name interface model =
    let
        ( updatedBlockedModules, readyModules ) =
            List.mapAccumR
                (updateBlockedModules name)
                model.blockedModules
                (Maybe.withDefault [] (EveryDict.get name model.reverseDependencies))

        readyList =
            readyModules
                |> List.filterMap identity
                |> List.filter (\( m, _ ) -> not (List.member m model.runningList))

        newCompletedInterfaces =
            EveryDict.insert name interface model.completedInterfaces

        runningList =
            List.filter ((/=) name) model.runningList
    in
        { model
            | blockedModules = updatedBlockedModules
            , completedInterfaces = newCompletedInterfaces
            , readyList = readyList
            , runningList = runningList
            , completedTasks = model.completedTasks + 1
        }


updateBlockedModules :
    CanonicalModule
    -> CanonicalModule
    -> EveryDict CanonicalModule BuildData
    -> ( EveryDict CanonicalModule BuildData, Maybe ( CanonicalModule, Location ) )
updateBlockedModules modul potentiallyFreedModule blockedModules =
    case EveryDict.get potentiallyFreedModule blockedModules of
        Nothing ->
            ( blockedModules, Nothing )

        Just { blocking, location } ->
            case List.filter ((/=) modul) blocking of
                [] ->
                    ( EveryDict.remove potentiallyFreedModule blockedModules
                    , Just ( potentiallyFreedModule, location )
                    )

                newBlocking ->
                    ( EveryDict.insert
                        potentiallyFreedModule
                        (BuildData newBlocking location)
                        blockedModules
                    , Nothing
                    )


buildModule : Model -> ( CanonicalModule, Location ) -> Task BM.Error ( CanonicalModule, Interface )
buildModule model ( modul, location ) =
    let
        packageName =
            Tuple.first modul.package

        path =
            Path.toSource location

        ifacePath =
            Path.toInterface model.cachePath modul

        objectPath =
            Path.toObjectFile model.cachePath modul
    in
        FileStorage.read path
            |> Task.map (Decode.decodeValue Decode.string)
            |> Task.andThen Task.fromResult
            |> Task.mapError BM.PackageProblem
            |> Task.andThen
                (\source ->
                    Compiler.compile packageName False source model.completedInterfaces
                        |> Task.mapError (BM.CompilerErrors path source)
                )
            |> Task.andThen
                (\( iface, elmo ) ->
                    Task.map2 (\_ _ -> ( modul, iface ))
                        (FileStorage.write
                            ifacePath
                            (Interface.encoder iface)
                            |> Task.mapError (\_ -> BM.CorruptedArtifact ifacePath)
                        )
                        (FileStorage.write
                            objectPath
                            (Encode.string elmo)
                            |> Task.mapError (\_ -> BM.CorruptedArtifact objectPath)
                        )
                )
