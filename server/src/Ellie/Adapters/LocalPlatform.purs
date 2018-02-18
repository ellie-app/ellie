module Ellie.Adapters.LocalPlatform
  ( initialize
  , destroy
  , compile
  , format
  ) where

import Prelude

import Control.Monad.Eff.Class as Eff
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept, runExceptT) as Except
import Control.Monad.IO as IO
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class (liftIO) as IO
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.FilePath (FilePath, (</>), (<.>))
import Data.FilePath as FilePath
import Data.Foreign (MultipleErrors)
import Data.Foreign.Generic (decodeJSON) as Foreign
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex (match) as Regex
import Data.String.Regex.Flags (noFlags, unicode) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Debug as Debug
import Ellie.Elm.Compiler.Error as Compiler
import Ellie.Elm.Package (Package)
import Ellie.Elm.Package.Description (Description(..))
import Ellie.Types.Workspace (Workspace(..))
import System.Elm as Elm
import System.FileSystem as FileSystem

initialize ∷ ∀ m. MonadIO m ⇒ MonadThrow Error m ⇒ m Workspace
initialize = IO.liftIO do
  root ← FileSystem.createTemporaryDirectory
  Eff.liftEff $ Console.log $ "Initializing workspace at location " <> root
  root
    # Elm.init
    >>= Either.either (error >>> throwError) pure
    # void
  rawDescription ←
    FileSystem.read $ root </> "elm-package.json"
  (Description description) ←
    rawDescription
      # Foreign.decodeJSON
      # Except.runExcept
      # Either.either (renderDecodeError >>> throwError) pure
  pure $ Workspace { location: root, packages: description.dependencies }
  where
    renderDecodeError ∷ MultipleErrors → Error
    renderDecodeError e =
      error $ "Corrupted description: " <> show e


destroy ∷ ∀ m. MonadIO m ⇒ Workspace → m Unit
destroy (Workspace workspace) = IO.liftIO do
  Eff.liftEff $ Console.log $ "Destroying workspace at location " <> workspace.location
  FileSystem.remove workspace.location
  pure unit


compile ∷ ∀ m. MonadIO m ⇒ MonadThrow Error m ⇒ String → String → Workspace → m (Array Compiler.Error)
compile elm html (Workspace workspace) = IO.liftIO do
  let elmPath = parseElmPath elm
  let htmlPath = "index.html"
  FileSystem.write (workspace.location </> elmPath) elm
  FileSystem.write (workspace.location </> htmlPath) html
  stderrOrStdout ←
    Elm.compile
      { root: workspace.location
      , entry: elmPath
      , debug: false
      , output: "build.js"
      }
  case stderrOrStdout of
    Left message →
      throwError $ error message
    Right text →
      let
        lines = String.split (Pattern "\n") text
        firstLine = Maybe.fromMaybe "" $ Array.head lines
      in
      if String.contains (Pattern "Successfully generated") firstLine then
        pure []
      else
        firstLine
          # Foreign.decodeJSON
          # Except.runExcept
          # Either.either (renderDecodeError >>> throwError) pure
  where
    elmModuleRegex ∷ Regex
    elmModuleRegex =
      Regex.unsafeRegex "module (([A-Z]{1}[a-zA-Z0-9]*\\.?)+) where" Regex.noFlags

    findElmModule ∷ String → String
    findElmModule source = Maybe.fromMaybe "Main" do
      matches ← Regex.match elmModuleRegex source
      secondMatch ← Array.index matches 1 
      secondMatch

    parseElmPath ∷ String → FilePath
    parseElmPath source =
      source
        # findElmModule
        # String.split (Pattern ".")
        # FilePath.joinParts
        # (_ <.> "elm")
    
    renderDecodeError ∷ MultipleErrors → Error
    renderDecodeError _ =
      error "Unparseable compiler result"


format ∷ ∀ m. MonadIO m ⇒ String → m (Either String String)
format code =
  IO.liftIO $ Elm.format code
