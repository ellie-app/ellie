module Ellie.SourceLoader
  ( load
  , Error(..)
  ) where

import Ellie.Prelude

import Control.Callback (CALLBACK, Callback)
import Control.Callback as Callback
import Control.Monad.Eff (Eff)
import Control.Monad.Task (Task)
import Control.Monad.Task as Task
import Data.Bifunctor (lmap)
import Data.Blob (BLOB, Blob)
import Data.Blob as Blob
import Data.Foreign as Foreign
import Data.Int (toNumber)
import Data.Url (Url)
import Data.Url as Url
import Elm.Compiler.Version as Compiler
import System.FileSystem (FILESYSTEM, FilePath, (<.>), (</>))
import System.FileSystem as FileSystem
import System.Http (HTTP)
import System.Http as Http


compilerSize :: Int
compilerSize =
  4730627


progressHandler :: ∀ e. Callback -> { loaded :: Int, total :: Int } -> Eff (callback :: CALLBACK | e) Unit
progressHandler callback { loaded } =
  (toNumber loaded) / (toNumber compilerSize)
    |> Foreign.toForeign
    |> Callback.run callback
    |> Task.runAndIgnore


scriptPath :: FilePath
scriptPath =
  "/compilers" </> show Compiler.version <.> "js"


fetchScript :: ∀ e. Callback -> Task (http :: HTTP | e) Error Blob
fetchScript onProgress =
  Url.crossOrigin "https://production-cdn.ellie-app.com" ["elm-compilers", show Compiler.version <.> "js" ] []
    |> Http.get
    |> Http.withHeader "Accept" "application/javascript"
    |> Http.withExpect Http.expectBlob
    |> Http.withProgress (\stuff -> progressHandler onProgress stuff)
    |> Http.send
    |> lmap DownloadFailure


saveBlob :: ∀ e. Blob -> Task (fileSystem :: FILESYSTEM | e) Error Blob
saveBlob blob =
  FileSystem.write scriptPath blob
    |> lmap SaveFailure
    |> map (const blob)


loadBlob :: ∀ e. Task (fileSystem :: FILESYSTEM | e) Error Blob
loadBlob =
  FileSystem.read scriptPath
    |> lmap ReadFailure


hasSavedBlob :: ∀ e x. Task (fileSystem :: FILESYSTEM | e) x Boolean
hasSavedBlob =
  FileSystem.exists scriptPath


data Error
  = SaveFailure FileSystem.Error
  | DownloadFailure Http.Error
  | ReadFailure FileSystem.Error

instance showError :: Show Error where
  show (SaveFailure error) = "Couldn't save compiler source to file system: " <> show error
  show (DownloadFailure error) = "Couldn't download compiler source from CDN: " <> show error
  show (ReadFailure error) = "Couldn't retrieve compiler source from file system: " <> show error


load :: ∀ e. Callback -> Task (http :: HTTP, fileSystem :: FILESYSTEM, blob :: BLOB | e) Error Url
load onProgress = 
  ifM
    hasSavedBlob
    (loadBlob >>= (Blob.createObjectUrl >>> Task.liftEff))
    (fetchScript onProgress >>= saveBlob >>= (Blob.createObjectUrl >>> Task.liftEff))
