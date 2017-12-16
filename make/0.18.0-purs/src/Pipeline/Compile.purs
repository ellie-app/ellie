module Pipeline.Compile where

import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Control.Monad.Task (AVar)
import Control.Monad.Task as Task
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map (delete, empty, insert, lookup, size, values) as Map
import Data.Map.Extra (insertWith, mapEitherWithKey, unsafeLookupFlipped) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (mapAccumR, traverse)
import Data.Tuple (Tuple(..))
import Data.Url (Url)
import Elm.Compiler as Compiler
import Elm.Compiler.Error as Error
import Elm.Compiler.Module.Interface (Interface)
import Elm.Package (Package)
import Report (Reporter)
import Report as Report
import System.FileSystem (FilePath)
import System.FileSystem as FileSystem
import TheMasterPlan.Build (BuildData(..), BuildGraph)
import TheMasterPlan.CanonicalModule (CanonicalModule)
import TheMasterPlan.CanonicalModule as CanonicalModule
import TheMasterPlan.Location (Location)


type Result =
  { source :: String
  , path :: FilePath
  , moduleId :: CanonicalModule
  , result :: Either (Array Error.Error) Compiler.Result
  }


type Interfaces =
  Map CanonicalModule Interface


type Env =
  { numTasks :: Int
  , compilerUrl :: Url
  , resultChannel :: AVar Result
  , doneChannel :: AVar (Either (Array Error.Error) Interfaces)
  , dependencies :: Map CanonicalModule (Array CanonicalModule)
  , reverseDependencies :: Map CanonicalModule (Array CanonicalModule)
  , modulesForGeneration :: Set CanonicalModule
  }


type State =
  { numActiveThreads :: Int
  , blockedModules :: Map CanonicalModule BuildData
  , results :: Either (Array Error.Error) Interfaces
  , numBuilt :: Int
  }



-- HELPERS for ENV and STATE


initEnv ::
  ∀  e x
  .  Url
  -> Array CanonicalModule
  -> Map CanonicalModule (Array CanonicalModule)
  -> BuildGraph
  -> Task Env
initEnv compilerUrl modulesForGeneration dependencies buildGraph = do
  let
    { completedInterfaces, blockedModules } =
      unwrap buildGraph

  resultChannel <- Task.makeEmptyVar
  doneChannel <- Task.makeEmptyVar

  pure
    { numTasks: Map.size blockedModules
    , reverseDependencies: reverseGraph dependencies
    , modulesForGeneration: Set.fromFoldable modulesForGeneration
    , compilerUrl
    , dependencies
    , resultChannel
    , doneChannel
    }


reverseGraph :: Map CanonicalModule (Array CanonicalModule) -> Map CanonicalModule (Array CanonicalModule)
reverseGraph graph =
  let
    insertDependency name dep reversedGraph =
      Map.insertWith (flip (<>)) dep [ name ] reversedGraph

    flipEdges name dependencies reversedGraph =
      Array.foldr (insertDependency name) reversedGraph dependencies
  in
    foldrWithIndex flipEdges Map.empty graph


initState ::
  ∀  e x
  .  Env
  -> BuildGraph
  -> Task State
initState env buildGraph =
  let
    { blockedModules: blocked, completedInterfaces } =
      unwrap buildGraph

    categorize name buildData@(BuildData { blocking, location }) =
      case blocking of
        [] -> Left (Tuple name location)
        _  -> Right buildData

    { left: readyModules, right: blockedModules } =
      Map.mapEitherWithKey categorize blocked

    readyList =
      readyModules
        |> Map.values
        |> Array.fromFoldable
  in do
    _ <-
      traverse (Task.fork <<< buildModule env completedInterfaces) readyList

    pure
      { numActiveThreads: Array.length readyList
      , numBuilt: 0
      , blockedModules
      , results: Right completedInterfaces
      }


build ::
  ∀  e x
  .  Url
  -> Reporter
  -> Package
  -> Array CanonicalModule
  -> Map CanonicalModule (Array CanonicalModule)
  -> BuildGraph
  -> Task (Either (Array Error.Error) Interfaces)
build compilerUrl reporter rootPkg modulesForGeneration dependencies summary = do
  env <- initEnv compilerUrl modulesForGeneration dependencies summary
  _ <- Task.fork (buildManager reporter env =<< initState env summary)
  Task.readVar env.doneChannel


updateLeft :: ∀ x a. x -> (x -> x) -> Either x a -> Either x a
updateLeft _ updater (Left x) = Left (updater x)
updateLeft default _ _ = Left default


buildManager :: Reporter -> Env -> State -> Task Unit
buildManager reporter env state =
  if state.numActiveThreads == 0 then do
    Task.putVar state.results env.doneChannel
  else do
    { source, path, moduleId, result } <-
      Task.takeVar env.resultChannel

    case result of
      Right { interface, js } -> do
        FileSystem.write
          (CanonicalModule.interfacePath moduleId)
          interface

        FileSystem.write
          (CanonicalModule.objectPath moduleId)
          js

        newState <- registerSuccess env state moduleId interface
        reporter <| Report.Compiling { total: env.numTasks, complete: newState.numBuilt }
        buildManager reporter env newState

      Left errors ->
        buildManager reporter env <|
          state
            { numActiveThreads = state.numActiveThreads - 1
            , results = updateLeft [] (_ <> errors) state.results
            }


registerSuccess :: Env -> State -> CanonicalModule -> Interface -> Task State
registerSuccess env state name interface =
    case state.results of
      Right completedInterfaces ->
        let
          { accum: updatedBlockedModules, value: readyModules } =
            mapAccumR
              (updateBlockedModules name)
              state.blockedModules
              (Maybe.fromMaybe [] (Map.lookup name env.reverseDependencies))

          readyList =
            Array.catMaybes readyModules

          newCompletedInterfaces =
            Map.insert name interface completedInterfaces
        in do
          _ <-
            traverse (Task.fork <<< buildModule env newCompletedInterfaces) readyList

          pure <|
            state
              { numActiveThreads = state.numActiveThreads - 1 + Array.length readyList
              , blockedModules = updatedBlockedModules
              , results = Right newCompletedInterfaces
              , numBuilt = state.numBuilt + 1
              }
      Left _ ->
        pure state


updateBlockedModules ::
  CanonicalModule
  -> Map CanonicalModule BuildData
  -> CanonicalModule
  -> { accum :: Map CanonicalModule BuildData
     , value :: Maybe (Tuple CanonicalModule Location)
     }
updateBlockedModules modul blockedModules potentiallyFreedModule =
  case Map.lookup potentiallyFreedModule blockedModules of
    Nothing ->
      { accum: blockedModules, value: Nothing }

    Just (BuildData { blocking, location }) ->
      case Array.filter (modul /= _) blocking of
        [] ->
          { accum: Map.delete potentiallyFreedModule blockedModules
          , value: Just (Tuple potentiallyFreedModule location)
          }

        newBlocking ->
          { accum:
              Map.insert
                potentiallyFreedModule
                (BuildData { blocking: newBlocking, location })
                blockedModules
          , value: Nothing
          }


buildModule :: Env -> Interfaces -> Tuple CanonicalModule Location -> Task Unit
buildModule env interfaces (Tuple modul location) =
  let
    packageName =
      modul
        |> unwrap
        |> _.package
        |> unwrap
        |> _.name

    moduleName =
      modul
        |> unwrap
        |> _.name

    path =
      location
        |> unwrap
        |> _.relativePath

    deps =
      modul
        |> Map.unsafeLookupFlipped env.dependencies
        |> map CanonicalModule.simplify

  in do
    source <-
      FileSystem.read path
        |> lmap (unwrap >>> _.path >>> BM.CorruptedArtifact)

    result <-
      Compiler.compile env.compilerUrl packageName source interfaces
        |> lmap ({ name: moduleName, source, message: _ } >>> BM.CompilerCrash)

    let
      finished =
        { moduleId: modul
        , source
        , path
        , result
        }

    Task.putVar finished env.resultChannel
