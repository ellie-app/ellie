module Pipeline.Crawl.Package
  ( dfsFromFiles
  , dfsFromExposedModules
  ) where

import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Control.Monad.Error.Class (throwError)
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (empty, fromFoldable, insert, lookup, member) as Map
import Data.Map.Extra (unionsWith) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Url (Url)
import Elm.Compiler as Compiler
import Elm.Compiler.Module.Name.Raw (Raw(..))
import Elm.Compiler.Module.Name.Raw as Raw
import Elm.Package.Description (Description(..))
import Elm.Package.Description as Description
import Elm.Package (Package(..))
import Elm.Package.Name (Name)
import Pipeline.Install.Solver (Solution)
import System.FileSystem (FilePath, (</>), (<.>))
import System.FileSystem as FileSystem
import TheMasterPlan.Package (PackageGraph(..), PackageData(..))


-- STATE and ENVIRONMENT


type Env =
  { sourceDirs :: Array FilePath
  , availableForeignModules :: Map Raw (Array Package)
  , allowNatives :: Boolean
  , packageName :: Name
  , compilerUrl :: Url
  }



initEnv :: Url -> FilePath -> Description -> Solution -> Task Env
initEnv compilerUrl root description@(Description d) solution = do
  availableForeignModules <- readAvailableForeignModules description solution
  pure <|
    { sourceDirs: map (root </> _) d.sourceDirs
    , availableForeignModules
    , allowNatives: d.natives
    , packageName: d.name
    , compilerUrl
    }




-- GENERIC CRAWLER


dfsFromFiles
  :: Url
  -> FilePath
  -> Solution
  -> Description
  -> Array FilePath
  -> Task { moduleNames :: Array Raw, packageGraph :: PackageGraph }
dfsFromFiles compilerUrl root solution desc filePaths = do
  env <- initEnv compilerUrl root desc solution
  info <- traverse (readPackageData env Nothing) filePaths

  let names = map _.name info
  let unvisited = Array.concatMap _.unvisited info
  let packageData = info |> map (\d -> Tuple d.name d.packageData) |> Map.fromFoldable
  let initialGraph = PackageGraph { data: packageData, natives: Map.empty, foreignDependencies: Map.empty }

  dfs env unvisited initialGraph
    |> map { moduleNames: names, packageGraph: _ }


dfsFromExposedModules
  :: Url
  -> FilePath
  -> Solution
  -> Description
  -> Task PackageGraph
dfsFromExposedModules compilerUrl root solution description@(Description d) = do
  env <- initEnv compilerUrl root description solution

  dfs
    env
    (map { parent: Nothing, name: _ } d.exposed)
    (PackageGraph { data: Map.empty, natives: Map.empty, foreignDependencies: Map.empty })




-- DEPTH FIRST SEARCH


type Unvisited =
  { parent :: Maybe Raw
  , name :: Raw
  }


dfs
  :: Env
  -> Array Unvisited
  -> PackageGraph
  -> Task PackageGraph
dfs env unvisited summary@(PackageGraph p) =
  case Array.uncons unvisited of
    Nothing ->
      pure summary

    Just { head, tail } ->
      if Map.member head.name p.data then
        dfs env tail summary
      else
        dfsHelp env head tail summary


dfsHelp
  :: Env
  -> Unvisited
  -> Array Unvisited
  -> PackageGraph
  -> Task PackageGraph
dfsHelp env { parent, name } unvisited summary@(PackageGraph p) = do
  filePaths <- find env.allowNatives name env.sourceDirs
  let toMatch = { filePaths, foreigns: Map.lookup name env.availableForeignModules }
  case toMatch of
    { filePaths: [Elm filePath], foreigns: Nothing } -> do
      { name: statedName, packageData, unvisited: newUnvisited } <-
        readPackageData env (Just name) filePath

      dfs
        env
        (newUnvisited <> unvisited)
        (PackageGraph <| p { data = Map.insert statedName packageData p.data })

    { filePaths: [JavaScript filePath], foreigns: Nothing } ->
      dfs
        env
        unvisited
        (PackageGraph <| p { natives = Map.insert name filePath p.natives })

    { filePaths: [], foreigns: Just [package] } ->
      dfs
        env
        unvisited
        (PackageGraph <| p { foreignDependencies = Map.insert name package p.foreignDependencies })

    { filePaths: [], foreigns: Nothing } ->
      throwError <| BM.ModuleNotFound name parent

    { filePaths: _, foreigns: maybePackages } ->
      throwError <|
        BM.ModuleDuplicates
          { name: name
          , parent: parent
          , local: map toFilePath filePaths
          , foreign: maybePackages |> map (map (unwrap >>> _.name)) |> Maybe.fromMaybe []
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
findHelp allowNatives locations moduleName@(Raw r) srcDirs =
    case Array.uncons srcDirs of
        Nothing ->
          pure locations

        Just { head: dir, tail: srcDirs' } ->
          let
            consIf true x xs = x : xs
            consIf false x xs = xs

            addElmPath locs =
              let elmPath = dir </> Raw.toPath moduleName <.> "elm"
              in FileSystem.exists elmPath |> map (\exists -> consIf exists (Elm elmPath) locs)

            addJsPath locs =
              let
                jsPath =
                  dir </> Raw.toPath moduleName <.> "js"

                jsExists =
                  case Array.uncons r of
                      Just { head: "Native", tail } -> FileSystem.exists jsPath
                      _ -> pure false
              in
                jsExists |> map (\exists -> consIf exists (JavaScript jsPath) locs)
          in do
            locations' <- addElmPath locations
            updatedLocations <-
              if allowNatives
                then addJsPath locations'
                else pure locations'
            findHelp allowNatives updatedLocations moduleName srcDirs'



-- READ and VALIDATE PACKAGE DATA for an ELM file


readPackageData
  :: Env
  -> Maybe Raw
  -> FilePath
  -> Task
      { name :: Raw
      , packageData :: PackageData
      , unvisited :: Array Unvisited
      }
readPackageData env maybeName filePath = do
  sourceCode <-
    FileSystem.read filePath
      |> lmap (unwrap >>> _.path >>> BM.CorruptedArtifact)

  result <-
    Compiler.parseDependencies env.compilerUrl env.packageName sourceCode
      |> lmap
          (\crashMessage ->
              BM.CompilerCrash
                  { name: Maybe.fromMaybe (Raw []) maybeName
                  , source: sourceCode
                  , message: crashMessage
                  }
          )

  { name, dependencies } <-
    case result of
      Left compileErrors -> throwError <| BM.CompilerErrors filePath sourceCode compileErrors
      Right stuff -> pure stuff

  checkName filePath name maybeName

  pure <|
    { name: name
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


readAvailableForeignModules :: Description -> Solution -> Task (Map Raw (Array Package))
readAvailableForeignModules desc solution = do
  visiblePackages <- allVisible desc solution
  maps <- traverse exposedModules visiblePackages
  pure <| Map.unionsWith (<>) maps


allVisible :: Description -> Solution -> Task (Array Package)
allVisible (Description d) solution = traverse getVersion visible
  where
  getVersion name =
    case Map.lookup name solution of
      Just version -> pure <| Package { name, version }
      Nothing -> throwError (BM.MissingPackage name)

  visible = map Tuple.fst (d.dependencies)


exposedModules :: Package -> Task (Map Raw (Array Package))
exposedModules package@(Package { name, version }) =
  let
    insert moduleName items =
      Map.insert moduleName [package] items
  in
    Description.load name version
      |> map (unwrap >>> _.exposed >>> Array.foldr insert Map.empty)
      |> lmap (unwrap >>> _.path >>> BM.CorruptedArtifact)
