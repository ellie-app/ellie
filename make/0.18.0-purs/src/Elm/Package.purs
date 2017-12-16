module Elm.Package
  ( Package(..)
  , fetchSources
  , saveSources
  , fetchArtifacts
  , saveArtifacts
  , markAsSaved
  , isSaved
  , fromTuple
  )
  where

import Ellie.Prelude

import Control.Monad.Task (Task)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, put, get)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Url (Url)
import Data.Url as Url
import Elm.Compiler.Version as Compiler
import Elm.Package.Name (Name(..))
import Elm.Package.Version (Version)
import System.FileSystem (FILESYSTEM, (</>))
import System.FileSystem as FileSystem
import System.Http (HTTP)
import System.Http as Http


newtype Package =
  Package
    { name :: Name
    , version :: Version
    }


derive instance newtypePackage :: Newtype Package _
derive instance eqPackage :: Eq Package
derive instance ordPackage :: Ord Package


instance foreignablePackage :: Foreignable Package where
  put (Package { name, version }) =
    Foreign.toForeign <|
      { name: put name
      , version: put version
      }
    
  get value =
    { name: _, version: _ }
      <$> (value ! "name" >>= get)
      <*> (value ! "version" >>= get)
      <#> Package


instance showPackage :: Show Package where
  show (Package { name, version }) =
    (show name) <> "@" <> (show version)



fromTuple :: Tuple Name Version -> Package
fromTuple (Tuple name version) =
  Package { name, version }


url :: Package -> Array String -> Url
url (Package { name: (Name name), version }) ending =
  Url.absolute
    (["package-artifacts/", name.user, name.project, show version] <> ending)
    []


fetchSources :: ∀ e. Package -> Task (http :: HTTP | e) Http.Error (Array (Tuple String String))
fetchSources package =
  url package [ "source.json" ]
    |> Http.get
    |> Http.withHeader "Content-Type" "application/json"
    |> Http.withExpect Http.expectJson
    |> Http.send
    |> map StrMap.toUnfoldable


saveSources :: ∀ e. Package -> Array (Tuple String String) -> Task (fileSystem :: FILESYSTEM | e) FileSystem.Error Unit
saveSources package@(Package { name: (Name name), version }) sources =
  let
    path =
      "/elm-stuff/packages"
          </> name.user
          </> name.project
          </> show version
  in
  traverse
    (\(Tuple key value) -> FileSystem.write (path </> key) value)
    sources
    <#> const unit


fetchArtifacts :: ∀ e. Package -> Task (http :: HTTP | e) Http.Error (Array (Tuple String String))
fetchArtifacts package@(Package { name: (Name name) }) =
  if name.user == "elm-lang" then
    url package [ "artifacts", show Compiler.version ]
      |> Http.get
      |> Http.withHeader "Content-Type" "application/json"
      |> Http.withExpect Http.expectJson
      |> Http.send
      |> map StrMap.toUnfoldable
  else
    pure []


saveArtifacts :: ∀ e. Package -> Array (Tuple String String) -> Task (fileSystem :: FILESYSTEM | e) FileSystem.Error Unit
saveArtifacts package@(Package { name: (Name name), version }) artifacts =
  let
    path =
      "elm-stuff/build-artifacts"
          </> show Compiler.version
          </> name.user
          </> name.project
          </> show version
  in
  traverse
    (\(Tuple key value) -> FileSystem.write (path </> key) value)
    artifacts
    <#> const unit


markAsSaved :: ∀ e. Package -> Task (fileSystem :: FILESYSTEM | e) FileSystem.Error Unit
markAsSaved package@(Package { name: (Name name), version }) =
  let
    path =
      "elm-stuff/packages"
        </> name.user
        </> name.project
        </> show version
        </> ".downloaded"
  in
  FileSystem.write path ""


isSaved :: ∀ x e. Package -> Task (fileSystem :: FILESYSTEM | e) x Boolean
isSaved package@(Package { name: (Name name), version }) =
  "elm-stuff/packages"
    </> name.user
    </> name.project
    </> show version
    </> ".downloaded"
    |> FileSystem.exists
