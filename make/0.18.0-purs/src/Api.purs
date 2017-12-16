module Api (start) where

import Ellie.Prelude

import BuildManager as BM
import Control.Callback (Callback)
import Control.Callback as Callback
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe as Eff
import Control.Monad.Error.Class (catchError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Task (AVar, AVAR)
import Control.Monad.Task as Task
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign as Foreign
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Read (read)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (match, parseFlags) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Url (Url)
import Ellie.SourceLoader as SourceLoader
import Elm.Compiler.Version (version) as Compiler
import Elm.Package.Constraint (Constraint)
import Elm.Package.Constraint as Constraint
import Elm.Package.Description (Description(..))
import Elm.Package.Name (Name(..))
import Elm.Package.Version (Version(..))
import Pipeline.Compile as Compile
import Pipeline.Crawl as Crawl
import Pipeline.Generate as Generate
import Pipeline.Install as Install
import Pipeline.Plan as Plan
import Report as Report
import System.FileSystem (FilePath, (<.>), (</>))
import System.FileSystem as FileSystem


type Dependency =
  { name :: String
  , version :: String
  }


readDependencies :: Array Dependency -> Either String (Array (Tuple Name Constraint))
readDependencies dependencies =
  traverse
    (\{ name, version } ->
      Tuple
        <$> read name
        <*> (read version <#> Constraint.forVersion)
    )
    dependencies


parseModuleName :: String -> FilePath
parseModuleName source =
  let
    moduleNameRegex =
      Regex.unsafeRegex
        "module ([a-zA-Z.]+) exposing"
        (Regex.parseFlags "g")

    matched = do
      matches <- Regex.match moduleNameRegex source
      first <- Array.head matches
      moduleName <- first
      moduleName
        |> String.split (Pattern ".")
        |> FileSystem.joinParts
        |> (_ <.> "elm")
        |> pure
  in
    Maybe.fromMaybe "Main.elm" matched


readDescription :: Array Dependency -> Either String Description
readDescription dependencies = do
  deps <- readDependencies dependencies
  pure <|
    Description
      { name: Name { user: "user", project: "project" }
      , repo: "https://github.com/user/project.git"
      , version: Version { major: 1, minor: 0, patch: 0 }
      , elmVersion: Constraint.forVersion Compiler.version
      , summary: "Generated description for Ellie"
      , license: "MIT"
      , sourceDirs: [ "/src" ]
      , exposed: []
      , natives: false
      , dependencies: deps
      }


compileService ::
  AVar { source :: String, dependencies :: Array Dependency }
    -> Callback
    -> Url
    -> BM.Task Unit
compileService compileChannel reportCallback compilerUrl =
  forever $
    Task.onError (show >>> Report.Failed >>> Report.reporter reportCallback) $ do
        { source, dependencies } <- Task.takeVar compileChannel
        
        let
          reporter =
            Report.reporter reportCallback

        let
          entryPath =
            "/src" </> parseModuleName source

        description <-
          dependencies
            |> readDescription
            |> lmap (\_ -> BM.CorruptedArtifact "elm-package.json")
            |> Task.fromEither

        source
          |> FileSystem.write entryPath
          |> lmap (\_ -> BM.CorruptedArtifact entryPath)


        reporter Report.InstallingPackages

        solution <-
          Install.getSolution description

        reporter Report.PlanningBuild

        { package, exposedModules, allModules, graph } <-
          Crawl.crawl compilerUrl "/src/Main.elm" description solution

        let
          dependencies =
            graph
              |> unwrap
              |> _.data
              |> map (unwrap >>> _.dependencies)

        buildSummary <-
          Plan.planBuild entryPath graph

        buildResult <-
          Compile.build
            compilerUrl
            reporter
            package
            allModules
            dependencies
            buildSummary

        case buildResult of
          Right interfaces -> do
            reporter Report.GeneratingCode

            code <-
              Generate.generate
                interfaces
                dependencies
                (graph |> unwrap |> _.natives)
                allModules

            pure unit

          Left errors -> do
            reporter <| Report.FinishedWithErrors errors
            pure unit

compileRequest ::
  ∀  e
  .  AVar { source :: String, dependencies :: Array Dependency }
  -> { source :: String, dependencies :: Array Dependency }
  -> Unit
compileRequest compileChannel stuff =
  Eff.unsafePerformEff $
    Task.runAndIgnore $
      Task.putVar stuff compileChannel

onCatch onReport e =
  case e of
    Left error -> Task.runAndIgnore $ Callback.run onReport (Foreign.toForeign error)
    Right ee ->
      case ee of
        Left eee -> Task.runAndIgnore $ Callback.run onReport (Foreign.toForeign (show eee))
        Right v -> Task.runAndIgnore $ Callback.run onReport (Foreign.toForeign v)

start :: { onReport :: Callback, onReady :: Callback } -> Unit
start { onReport, onReady } =
  Eff.unsafePerformEff $
    Task.runAndCatch (onCatch onReport) $ do
      compileChannel <- Task.makeEmptyVar
      compilerUrl <-
        SourceLoader.load onReport
          |> lmap BM.CompilerInstallationError
      thread <- Task.fork $ compileService compileChannel onReport compilerUrl
      Callback.run onReady (Foreign.toForeign (compileRequest compileChannel))
