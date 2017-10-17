module Pipeline.Plan where

import Path
import Prelude
import BuildManager as BM
import Data.Foldable (foldM)
import Control.Monad.Aff (Aff)
import Control.Monad.Except (throwError)
import Data.Array as Array
import Data.Argonaut (decodeJson)
import Data.Bifunctor (lmap, rmap)
import Data.DateTime.Instant (Instant)
import Data.Graph as Graph
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra ((!))
import Data.Map.Extra as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Elm.Compiler.Module as Module
import System.FileSystem (FILESYSTEM, FilePath)
import System.FileSystem as FileSystem
import TheMasterPlan (CanonicalModule(..), Location(..), ProjectGraph(ProjectGraph), ProjectData(..), BuildGraph(..), BuildData(..))
import Data.Either (Either(..))

planBuild :: BM.Config -> ProjectGraph Location -> BM.Task BuildGraph
planBuild config  (ProjectGraph graph) = do
  enhancedData <- Map.traverseWithKey (enhanceData BM.artifactDirectory) graph.data
  filteredData <- loadCachedInterfaces enhancedData
  pure (toBuildGraph filteredData)


type EnhancedGraph =
  Map CanonicalModule (ProjectData (Tuple Location (Maybe (Tuple FilePath Instant))))


type InterfacedGraph =
  Map CanonicalModule (ProjectData (Either Location Module.Interface))


enhanceData
  :: FilePath
  -> CanonicalModule
  -> ProjectData Location
  -> BM.Task (ProjectData (Tuple Location (Maybe (Tuple FilePath Instant))))
enhanceData artifactRoot moduleId (ProjectData { location, dependencies }) =
  if isMain moduleId then
    pure $ ProjectData { location: Tuple location Nothing, dependencies }
  else do
    let interfacePath = Path.toInterface artifactRoot moduleId
    let sourcePath = Path.toSource location
    interfaceInfo <- liftAff $ getFreshInterfaceInfo sourcePath interfacePath
    pure $ ProjectData { location: Tuple location interfaceInfo, dependencies }


-- getFreshInterfaceInfo
--   :: forall e
--   .  FilePath
--   -> FilePath
--   -> Aff (fileSystem :: FILESYSTEM | e) (Maybe (Tuple FilePath Instant))
-- getFreshInterfaceInfo sourcePath interfacePath = do
--   exists <- FileSystem.exists interfacePath
--   if exists then
--     pure Nothing
--   else
--     pure Nothing
    -- sourceTime <- FileSystem.modified sourcePath
    -- interfaceTime <- FileSystem.modified interfacePath
    -- if sourceTime <= interfaceTime then
    --   pure $ Just (interfacePath, interfaceTime)
    -- else
    --   pure Nothing


isMain :: CanonicalModule -> Boolean
isMain (CanonicalModule { name: Module.Raw names }) =
  names == ["Main"]



-- FILTER STALE INTERFACES -- have files become stale due to other changes?


loadCachedInterfaces :: EnhancedGraph -> BM.Task InterfacedGraph
loadCachedInterfaces summary = do
  sortedNames <- topologicalSort (Map.map .dependencies summary)
  foldM (updateFromCache summary) Map.empty sortedNames


updateFromCache
  :: EnhancedGraph
  -> InterfacedGraph
  -> CanonicalModule
  -> BM.Task InterfacedGraph
updateFromCache enhancedGraph interfacedGraph moduleName = do
  trueLocation <- getTrueLocation
  pure $
    Map.insert
      moduleName
      (ProjectData { location: trueLocation, dependencies })
      interfacedGraph
  where
    (ProjectData { location: Tuple location maybeInterfaceInfo, dependencies }) =
      enhancedGraph ! moduleName

    getTrueLocation =
      case maybeInterfaceInfo of
        Nothing ->
          pure $ Left location

        Just (Tuple interfacePath time) ->
          if Array.all (isCacheValid time enhancedGraph interfacedGraph) dependencies then do
            json <- FileSystem.read interfacePath
            decodeJson json # lmap (const $ BM.CorruptedArtifact interfacePath)
          else
            pure $ Left location


isCacheValid
  :: Instant
  -> EnhancedGraph
  -> InterfacedGraph
  -> CanonicalModule
  -> Boolean
isCacheValid time enhancedGraph interfacedGraph possiblyNativeName =
  case filterNativeDeps possiblyNativeName of
    Nothing ->
      true

    Just name ->
      let
        interfaceLocation = _.location $ unwrap $ (interfacedGraph ! name)
        enhancedLocation = _.location $ unwrap $ (enhancedGraph ! name)
      in
        case Tuple interfaceLocation enhancedLocation of
          Right (Tuple _ (Tuple _ (Just (Tuple _ depTime)))) ->
            depTime <= time

          Left _ ->
            false



-- FILTER DEPENDENCIES -- which modules actually need to be compiled?


-- toBuildGraph
--     :: InterfacedGraph
--     -> BuildGraph
-- toBuildGraph summary =
--   BuildGraph
--     { blockedModules = Map.map (toBuildData interfaces) locations
--     , completedInterfaces = interfaces
--     }
--   where
--     (locations, interfaces) =
--         Map.mapEither divide summary
--
--     divide (ProjectData either deps) =
--         case either of
--           Left location ->
--               Left (ProjectData location deps)
--
--           Right interface ->
--               Right interface

-- toBuildData
--   :: Map.Map CanonicalModule Module.Interface
--   -> ProjectData Location
--   -> BuildData
-- toBuildData interfaces (ProjectData location dependencies) =
--     BuildData blocking location
--   where
--     blocking =
--         Maybe.mapMaybe filterDeps dependencies
--
--     filterDeps :: CanonicalModule -> Maybe CanonicalModule
--     filterDeps deps =
--         filterCachedDeps interfaces =<< filterNativeDeps deps


-- filterCachedDeps
--   :: Map.Map CanonicalModule Module.Interface
--   -> CanonicalModule
--   -> Maybe CanonicalModule
-- filterCachedDeps interfaces name =
--     case Map.lookup name interfaces of
--       Just _interface -> Nothing
--       Nothing -> Just name


filterNativeDeps :: CanonicalModule -> Maybe CanonicalModule
filterNativeDeps name =
    case name of
      CanonicalModule { package, name: (Module.Raw ["Native", _]) } ->
          Nothing

      _ ->
          Just name



-- SORT GRAPHS / CHECK FOR CYCLES


-- topologicalSort :: Map.Map CanonicalModule [CanonicalModule] -> BM.Task [CanonicalModule]
-- topologicalSort dependencies =
--   let
--     toNode (name, deps) =
--       (name, name, deps)
--
--     components =
--       Graph.stronglyConnComp (map toNode (Map.toList dependencies))
--   in
--     mapM errorOnCycle components
--
--
-- errorOnCycle :: Graph.SCC CanonicalModule -> BM.Task CanonicalModule
-- errorOnCycle scc =
--   case scc of
--     Graph.AcyclicSCC name ->
--       return name
--
--     Graph.CyclicSCC cycle ->
--       throwError (BM.Cycle cycle)
