module Api (start, clearElmStuff) where

import Ellie.Prelude

import BuildManager as BM
import Control.Callback (Callback)
import Control.Callback as Callback
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe as Eff
import Control.Monad.Error.Class (catchError)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Rec.Class (forever)
import Control.Monad.Task (AVar, AVAR)
import Control.Monad.Task as Task
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Blob as Blob
import Data.Either (Either(..))
import Data.Foreign as Foreign
import Data.Foreign.Class as Foreign
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Read (read)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (match) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Url (Url)
import Ellie.SourceLoader as SourceLoader
import Elm.Compiler (Compiler)
import Elm.Compiler as Compiler
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
        Regex.noFlags

    matched = do
      matches <- Regex.match moduleNameRegex source
      first <- Array.index matches 1
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
    -> Compiler
    -> BM.Task Unit
compileService compileChannel reportCallback compiler =
  forever $ do
    { source, dependencies } <-
      Task.takeVar compileChannel
        |> lmap (Exception.message >>> BM.ImpossibleError)

    let
      reporter =
        Report.reporter reportCallback

    let
      entryPath =
        "/src" </> parseModuleName source
    
    flip catchError (onCatch reportCallback) $
      Task.finally (FileSystem.remove entryPath |> lmap (const (BM.CorruptedArtifact entryPath))) $ do
          description <-
            dependencies
              |> readDescription
              |> lmap (\_ -> BM.CorruptedArtifact "elm-package.json")
              |> Task.fromEither

          source
            |> FileSystem.write entryPath
            |> lmap (\_ -> BM.CorruptedArtifact entryPath)


          reporter Report.InstallingPackages
            |> lmap (Exception.message >>> BM.ImpossibleError)


          solution <-
            Install.getSolution description

          reporter Report.PlanningBuild
            |> lmap (Exception.message >>> BM.ImpossibleError)


          { package, exposedModules, allModules, graph } <-
            Crawl.crawl compiler entryPath description solution

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
              compiler
              reporter
              package
              allModules
              dependencies
              buildSummary

          case buildResult of
            Right interfaces -> do
              reporter Report.GeneratingCode
                |> lmap (Exception.message >>> BM.ImpossibleError)

              maybeBlob <-
                Generate.generate
                  interfaces
                  dependencies
                  (graph |> unwrap |> _.natives)
                  allModules

              case maybeBlob of
                Just codeBlob -> do
                  codeBlob
                    |> Report.Success
                    |> reporter
                    |> lmap (Exception.message >>> BM.ImpossibleError)

                Nothing ->
                  BM.ImpossibleError "No modules were generated for some reason. This should never happen"
                      |> show
                      |> Report.Failed
                      |> reporter
                      |> lmap (Exception.message >>> BM.ImpossibleError)


            Left errors ->
              errors
                |> Report.FinishedWithErrors
                |> reporter
                |> lmap (Exception.message >>> BM.ImpossibleError)

compileRequest ::
  ∀  e
  .  AVar { source :: String, dependencies :: Array Dependency }
  -> { source :: String, dependencies :: Array Dependency }
  -> Unit
compileRequest compileChannel stuff =
  Eff.unsafePerformEff $
    map (const unit) $
      Task.fork $
        Task.putVar stuff compileChannel

onCatch :: Callback -> BM.Error -> BM.Task Unit
onCatch onReport e =
  case e of
    BM.CompilerErrors _ _ errors ->
      Report.reporter onReport (Report.FinishedWithErrors errors)
        |> lmap (Exception.message >>> BM.ImpossibleError)

    _ ->
      Report.reporter onReport (Report.Failed (show e))
        |> lmap (Exception.message >>> BM.ImpossibleError)


start :: { onReport :: Callback, onReady :: Callback } -> Unit
start { onReport, onReady } =
  Eff.unsafePerformEff $
    map (const unit) $
      Task.fork $
        flip catchError (onCatch onReport) $ do
          compileChannel <-
            Task.makeEmptyVar
              |> lmap (Exception.message >>> BM.ImpossibleError)

          compiler <-
            SourceLoader.load onReport
              |> lmap BM.CompilerInstallationError
              >>= (Compiler.makeCompiler >>> lmap BM.ImpossibleError)
          
          thread <-
            compileService compileChannel onReport compiler
              |> Task.fork
              |> Task.liftEff "Api.compileService >>> Task.fork"
              |> lmap (Exception.message >>> BM.ImpossibleError)

          Callback.run onReady (Foreign.toForeign (compileRequest compileChannel))
            |> Task.liftEff "Callback.run"
            |> lmap (Exception.message >>> BM.ImpossibleError)


clearElmStuff :: Unit -> Unit
clearElmStuff _ =
  FileSystem.remove "/elm-stuff"
    |> Task.fork
    |> map (const unit)
    |> Eff.unsafePerformEff
