module Pipeline.Plan where

import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Control.Monad.Task as Task
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Graph as Graph
import Data.Map (Map)
import Data.Map (empty, insert, lookup, mapWithKey) as Map
import Data.Map.Extra (mapEither, traverseWithKey, unsafeLookup) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Elm.Compiler.Module.Name.Raw (Raw(..))
import Elm.Compiler.Module.Interface (Interface)
import Elm.Compiler.Module.Interface as Interface
import System.FileSystem (FilePath)
import System.FileSystem as FileSystem
import TheMasterPlan.Build (BuildGraph(..), BuildData(..))
import TheMasterPlan.CanonicalModule (CanonicalModule(..))
import TheMasterPlan.CanonicalModule as CanonicalModule
import TheMasterPlan.Location (Location)
import TheMasterPlan.Project (ProjectGraph(..), ProjectData(..))


type InterfaceInfo =
  { path :: FilePath
  , modified :: Instant
  }


type LocationInfo =
  { location :: Location
  , interfaceInfo :: Maybe InterfaceInfo
  }


type EnhancedGraph =
  Map
    CanonicalModule
    (ProjectData LocationInfo)


type InterfacedGraph =
  Map
    CanonicalModule
    (ProjectData (Either Location Interface))


planBuild :: FilePath -> ProjectGraph Location -> Task BuildGraph
planBuild entry (ProjectGraph p) =
  p.data
    |> Map.traverseWithKey enhanceData
    >>= loadCachedInterfaces
    |> map toBuildGraph


enhanceData ::
  CanonicalModule
  -> ProjectData Location
  -> Task (ProjectData LocationInfo)
enhanceData moduleId (ProjectData { location, dependencies }) =
    if isMain moduleId then
      pure <|
        ProjectData
          { location: { location, interfaceInfo: Nothing }
          , dependencies
          }
    else do
      let interfacePath = CanonicalModule.interfacePath moduleId
      let sourcePath = location |> unwrap |> _.relativePath
      interfaceInfo <- getFreshInterfaceInfo sourcePath interfacePath
      pure <|
        ProjectData
          { location: { location, interfaceInfo }
          , dependencies
          }


isMain :: CanonicalModule -> Boolean
isMain (CanonicalModule { name: Raw [ "Main" ] }) = true
isMain _ = false


getFreshInterfaceInfo ::
  FilePath
  -> FilePath
  -> Task (Maybe InterfaceInfo)
getFreshInterfaceInfo sourcePath interfacePath = do
  interfaceExists <- FileSystem.exists interfacePath
  if not interfaceExists
    then
      pure Nothing
    else do
      modifiedTimes <-
        Tuple
          <$> FileSystem.modified sourcePath
          <*> FileSystem.modified interfacePath
          |> lmap (const (BM.CorruptedArtifact sourcePath))

      case modifiedTimes of
        Tuple (Just sourceTime) (Just interfaceTime) ->
          if sourceTime <= interfaceTime then
            pure <| Just { path: interfacePath, modified: interfaceTime }
          else
            pure <| Nothing
        _ ->
          pure <| Nothing


loadCachedInterfaces :: EnhancedGraph -> Task InterfacedGraph
loadCachedInterfaces summary =
  summary
    |> Map.mapWithKey (\k (ProjectData v) -> v.dependencies)
    |> topologicalSort
    >>= Array.foldM (updateFromCache summary) Map.empty


updateFromCache ::
  EnhancedGraph
  -> InterfacedGraph
  -> CanonicalModule
  -> Task InterfacedGraph
updateFromCache enhancedGraph interfacedGraph moduleName =
  let
    (ProjectData pd) =
      Map.unsafeLookup moduleName enhancedGraph

    { location, interfaceInfo } =
      pd.location

    getTrueLocation =
      case interfaceInfo of
        Nothing ->
          pure <| Left location

        Just { path, modified } ->
          if Array.all (isCacheValid modified enhancedGraph) pd.dependencies then
            path
              |> Interface.read
              |> lmap (\_ -> BM.CorruptedArtifact path)
              |> map Right
          else
            pure <| Left location
    in
    getTrueLocation
      |> map
          (\trueLocation ->
            Map.insert
              moduleName
              (ProjectData { location: trueLocation, dependencies: pd.dependencies })
              interfacedGraph
          )


isCacheValid ::
  Instant
  -> EnhancedGraph
  -> CanonicalModule
  -> Boolean
isCacheValid time enhancedGraph possiblyNativeName =
  case filterNativeDeps possiblyNativeName of
    Nothing ->
      true

    Just name ->
      if CanonicalModule.isElmLang name then
        true
      else
        case Map.unsafeLookup name enhancedGraph of
          ProjectData { location: { interfaceInfo: Just { modified } } } ->
            modified <= time

          _ ->
            false


toBuildGraph :: InterfacedGraph -> BuildGraph
toBuildGraph summary =
  let
    { left: locations, right: interfaces } =
      Map.mapEither divide summary
  in
    BuildGraph
      { blockedModules: map (toBuildData interfaces) locations
      , completedInterfaces: interfaces
      }


divide :: ProjectData (Either Location Interface) -> Either (ProjectData Location) Interface
divide (ProjectData { location, dependencies }) =
  case location of
    Left l -> Left (ProjectData { location: l, dependencies })
    Right i -> Right i


toBuildData ::
  Map CanonicalModule Interface
  -> ProjectData Location
  -> BuildData
toBuildData interfaces (ProjectData { location, dependencies }) =
  let
    blocking =
      Array.mapMaybe filterDeps dependencies

    filterDeps deps =
      filterNativeDeps deps
        >>= (filterCachedDeps interfaces)
  in
    BuildData { blocking, location }


filterCachedDeps ::
  Map CanonicalModule Interface
  -> CanonicalModule
  -> Maybe CanonicalModule
filterCachedDeps interfaces name =
  case Map.lookup name interfaces of
    Just interface -> Nothing
    Nothing -> Just name


filterNativeDeps :: CanonicalModule -> Maybe CanonicalModule
filterNativeDeps cm@(CanonicalModule { name: Raw n }) =
  case Array.head n of
    Just "Native" -> Nothing
    _ -> Just cm


topologicalSort ::
  Map CanonicalModule (Array CanonicalModule)
    -> Task (Array CanonicalModule)
topologicalSort dependencies =
  dependencies
    |> Graph.fromAdjacency
    |> Graph.topologicalSort
    |> lmap (Array.head >>> map Graph.nodes >>> Maybe.fromMaybe [] >>> BM.Cycle)
    |> Task.fromEither
