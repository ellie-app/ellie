module Pipeline.Crawl.Package (dfsFromFiles, dfsFromExposedModules) where 

import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Control.Monad.Error.Class (throwError)
import Control.Monad.Task as Task
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.Foreign as Foreign
import Data.Foreign.Class as Foreign
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Url (Url)
import Elm.Compiler (Compiler)
import Elm.Compiler as Compiler
import Elm.Compiler.Module.Name.Raw (Raw(..))
import Elm.Compiler.Module.Name.Raw as Raw
import Elm.Package (Package(..))
import Elm.Package as Package
import Elm.Package.Description (Description(..))
import Elm.Package.Description as Description
import Elm.Package.Name (Name)
import Elm.Package.Name as Name
import Elm.Package.Version (Version)
import Elm.Package.Version as Version
import Pipeline.Install.Solution (Solution)
import System.FileSystem (FilePath, (</>), (<.>))
import System.FileSystem as FileSystem
import TheMasterPlan.Package (PackageData(..), PackageGraph(..))
import TheMasterPlan.Package as PackageGraph

-- STATE and ENVIRONMENT


type Env =
    { sourceDirs :: Array FilePath
    , availableForeignModules :: Map Raw (Array Package)
    , allowNatives :: Boolean
    , packageName :: Name
    , compiler :: Compiler
    }



initEnv :: Compiler -> FilePath -> Description -> Solution -> Task Env
initEnv compiler root desc@(Description d) solution = do
  availableForeignModules <- readAvailableForeignModules desc solution
  pure
    { sourceDirs: map (root </> _) d.sourceDirs
    , allowNatives: d.natives
    , packageName: d.name
    , availableForeignModules
    , compiler
    }



-- GENERIC CRAWLER


dfsFromFiles
  :: Compiler
  -> FilePath
  -> Solution
  -> Description
  -> Array FilePath
  -> Task { moduleNames :: Array Raw, packageGraph :: PackageGraph }
dfsFromFiles compiler root solution desc filePaths = do
    env <- initEnv compiler root desc solution
    infos <- traverse (readPackageData env Nothing) filePaths
    let names = map _.name infos
    let unvisited = Array.concatMap (_.unvisited) infos
    let packageData = Map.fromFoldable (map (\{ name, packageData } -> Tuple name packageData) infos)
    let initialGraph = PackageGraph { data: packageData, natives: Map.empty, foreignDependencies: Map.empty }
    packageGraph <- dfs env unvisited initialGraph
    pure { moduleNames: names, packageGraph }


dfsFromExposedModules
  :: Compiler
  -> FilePath
  -> Solution
  -> Description
  -> Task PackageGraph
dfsFromExposedModules compiler root solution desc@(Description d) = do
  env <- initEnv compiler root desc solution
  dfs
    env
    (map { parent: Nothing, name: _ } d.exposed)
    (PackageGraph { data: Map.empty, natives: Map.empty, foreignDependencies: Map.empty })



-- DEPTH FIRST SEARCH


type Unvisited =
    { parent :: Maybe Raw
    , name :: Raw
    }


dfs :: Env -> Array Unvisited -> PackageGraph -> Task PackageGraph
dfs env unvisited summary@(PackageGraph p) =
  case Array.uncons unvisited of
    Nothing ->
      pure summary

    Just { head: next, tail: rest } ->
      if Map.member next.name p.data then
        dfs env rest summary
      else
        dfsHelp env next rest summary


dfsHelp :: Env -> Unvisited -> Array Unvisited -> PackageGraph -> Task PackageGraph
dfsHelp env { parent, name } unvisited summary@(PackageGraph s) = do
  filePaths <- find env.allowNatives name env.sourceDirs
  case Tuple filePaths (Map.lookup name env.availableForeignModules) of
    Tuple [Elm filePath] Nothing -> do
      { name: statedName, packageData, unvisited: newUnvisited } <-
        readPackageData env (Just name) filePath

      dfs
        env
        (newUnvisited <> unvisited)
        (PackageGraph (s { data = Map.insert statedName packageData s.data }))

    Tuple [JavaScript filePath] Nothing ->
      dfs
        env
        unvisited
        (PackageGraph (s { natives = Map.insert name filePath s.natives }))

    Tuple [] (Just [package]) ->
      dfs
        env
        unvisited
        (PackageGraph (s { foreignDependencies = Map.insert name package s.foreignDependencies }))

    Tuple [] Nothing ->
      throwError <| BM.ModuleNotFound name parent

    Tuple _ maybePackages ->
      throwError <|
        BM.ModuleDuplicates
          { name: name
          , parent: parent
          , local: map toFilePath filePaths
          , foreign:
              maybePackages
                |> map (map (unwrap >>> _.name))
                |> Maybe.fromMaybe []
          }


-- FIND LOCAL FILE PATH


data CodePath
    = Elm FilePath
    | JavaScript FilePath


toFilePath :: CodePath -> FilePath
toFilePath codePath =
  case codePath of
    Elm file -> file
    JavaScript file -> file


find :: Boolean -> Raw -> Array FilePath -> Task (Array CodePath)
find allowNatives moduleName sourceDirs =
  findHelp allowNatives [] moduleName sourceDirs

findHelp :: Boolean -> Array CodePath -> Raw -> Array FilePath -> Task (Array CodePath)
findHelp allowNatives locations moduleName srcDirs =
  case Array.uncons srcDirs of
    Nothing ->
      pure locations

    Just { head: dir, tail: srcDirs } ->
      let
        consIf :: ∀ a. Boolean -> a -> Array a -> Array a
        consIf true x xs = x : xs
        consIf false _ xs = xs

        addElmPath :: Array CodePath -> Task (Array CodePath)
        addElmPath locs =
          let
            elmPath = dir </> Raw.toPath moduleName <.> "elm"
          in
            FileSystem.exists elmPath
              |> map (\e -> consIf e (Elm elmPath) locs)

        addJsPath :: Array CodePath -> Task (Array CodePath)
        addJsPath locs =
          let
            jsPath = dir </> Raw.toPath moduleName <.> "js"
          in
            if Raw.isNative moduleName then
                FileSystem.exists jsPath
                  |> map (\e -> consIf e (JavaScript jsPath) locs)
            else
              pure locs
      in do
        locations' <-
          addElmPath locations

        locations'' <-
          if allowNatives
            then addJsPath locations'
            else pure locations'
        
        findHelp allowNatives locations'' moduleName srcDirs



-- READ and VALIDATE PACKAGE DATA for an ELM file


readPackageData
  :: Env
  -> Maybe Raw
  -> FilePath
  -> Task { name :: Raw, packageData :: PackageData, unvisited :: Array Unvisited }
readPackageData env maybeName filePath = do
  sourceCode <-
    FileSystem.read filePath
      |> lmap (unwrap >>> _.path >>> BM.PackageProblem)
 
  result <-
     Compiler.parseDependencies env.compiler env.packageName sourceCode
        |> lmap ({ name: Maybe.fromMaybe (Raw []) maybeName, source: sourceCode, message: _ } >>> BM.CompilerCrash)

  { name, dependencies } <-
    result
      |> lmap (BM.CompilerErrors filePath sourceCode)
      |> Task.fromEither

  checkName filePath name maybeName

  pure
    { name
    , packageData: PackageData { path: filePath, dependencies }
    , unvisited: map { parent: Just name, name: _ } dependencies
    }


checkName :: FilePath -> Raw -> Maybe Raw -> Task Unit
checkName path nameFromSource maybeName =
  case maybeName of
    Just nameFromPath ->
      if nameFromPath /= nameFromSource then
        throwError <| BM.ModuleName { path, actualName: nameFromSource, expectedName: nameFromPath }
      else
        pure unit
    Nothing ->
      pure unit


-- FOREIGN MODULES -- which ones are available, who exposes them?


readAvailableForeignModules
  :: Description
  -> Solution
  -> Task (Map Raw (Array Package))
readAvailableForeignModules desc solution = do
  visiblePackages <- allVisible desc solution
  exposed <- traverse exposedModules visiblePackages
  pure <| Map.unionsWith (<>) exposed


allVisible
  :: Description
  -> Solution
  -> Task (Array Package)
allVisible desc@(Description d) solution =
  let
    getVersion name =
      case Map.lookup name solution of
        Just version -> pure (Package { name, version })
        Nothing -> throwError (BM.MissingPackage name)

    visible =
        map Tuple.fst (d.dependencies)
  in
    traverse getVersion visible


exposedModules
  :: Package
  -> Task (Map Raw (Array Package))
exposedModules (Package { name, version }) =
  let
    insert moduleName dict =
      Map.insert moduleName [Package { name, version }] dict
  in
    Description.load name version
      |> lmap (unwrap >>> _.path >>> BM.PackageProblem)
      |> rmap (unwrap >>> _.exposed >>> Array.foldr insert Map.empty)