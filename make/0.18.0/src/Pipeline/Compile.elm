module Pipeline.Compile exposing (..)

import Data.Either as Either exposing (Either(..))
import Data.FilePath as FilePath exposing (FilePath)
import Data.HashDict as HashDict exposing (HashDict)
import Data.HashSet as HashSet exposing (HashSet)
import Elm.Compiler as Compiler
import Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Elm.Make.BuildData as BuildData exposing (BuildData)
import Elm.Make.BuildGraph as BuildGraph exposing (BuildGraph)
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Elm.Make.Error as BM
import Elm.Make.Location as Location exposing (Location)
import Extra.List as List
import Extra.Task as Task
import FileStorage as FileStorage
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Path as Path
import Task exposing (Task)


type alias Model =
    { numTasks : Int
    , dependencies : HashDict CanonicalModule (List CanonicalModule)
    , reverseDependencies : HashDict CanonicalModule (List CanonicalModule)
    , cachePath : FilePath
    , exposedModules : HashSet CanonicalModule
    , modulesForGeneration : HashSet CanonicalModule
    , blockedModules : HashDict CanonicalModule BuildData
    , completedInterfaces : HashDict CanonicalModule Interface
    , readyList : List ( CanonicalModule, Location )
    , runningList : List CanonicalModule
    , completedTasks : Int
    }



-- HELPERS for ENV and STATE


init :
    FilePath
    -> HashSet CanonicalModule
    -> List CanonicalModule
    -> HashDict CanonicalModule (List CanonicalModule)
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
            HashDict.mapEitherWithKey categorize buildGraph.blockedModules

        readyList =
            HashDict.values readyModules
    in
    { numTasks = HashDict.size buildGraph.blockedModules
    , dependencies = dependencies
    , reverseDependencies = reverseGraph dependencies
    , cachePath = cachePath
    , exposedModules = exposedModules
    , modulesForGeneration = HashSet.fromList modulesForGeneration
    , blockedModules = blockedModules
    , completedInterfaces = buildGraph.completedInterfaces
    , readyList = readyList
    , runningList = []
    , completedTasks = 0
    }


reverseGraph : HashDict CanonicalModule (List CanonicalModule) -> HashDict CanonicalModule (List CanonicalModule)
reverseGraph graph =
    let
        insertDependency name dep reversedGraph =
            HashDict.insertWith (flip (++)) dep [ name ] reversedGraph

        flipEdges name dependencies reversedGraph =
            List.foldr (insertDependency name) reversedGraph dependencies
    in
    HashDict.foldr flipEdges HashDict.empty graph


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
                (Maybe.withDefault [] (HashDict.get name model.reverseDependencies))

        readyList =
            readyModules
                |> List.filterMap identity
                |> List.filter (\( m, _ ) -> not (List.member m model.runningList))

        newCompletedInterfaces =
            HashDict.insert name interface model.completedInterfaces

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
    -> HashDict CanonicalModule BuildData
    -> ( HashDict CanonicalModule BuildData, Maybe ( CanonicalModule, Location ) )
updateBlockedModules modul potentiallyFreedModule blockedModules =
    case HashDict.get potentiallyFreedModule blockedModules of
        Nothing ->
            ( blockedModules, Nothing )

        Just { blocking, location } ->
            case List.filter ((/=) modul) blocking of
                [] ->
                    ( HashDict.remove potentiallyFreedModule blockedModules
                    , Just ( potentiallyFreedModule, location )
                    )

                newBlocking ->
                    ( HashDict.insert
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
