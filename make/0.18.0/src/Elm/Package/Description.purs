module Elm.Package.Description
  ( Description(..)
  , load
  , save
  , isSaved
  , fetch
  )
  where

import Ellie.Prelude

import Constants as Constants
import Control.Monad.Except (except)
import Control.Monad.Task (Task)
import Data.Bifunctor (lmap, bimap)
import Data.Foreign (Foreign, F)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, get, put)
import Data.Foreign.Index ((!))
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (class Newtype)
import Data.Read (read)
import Data.StrMap as StrMap
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Url as Url
import Elm.Compiler.Module.Name.Raw (Raw)
import Elm.Compiler.Version as Compiler
import Elm.Package.Constraint (Constraint)
import Elm.Package.Name (Name(..))
import Elm.Package.Version (Version)
import System.FileSystem (FILESYSTEM, FilePath, (</>))
import System.FileSystem as FileSystem
import System.Http (HTTP)
import System.Http as Http

newtype Description =
  Description
    { name :: Name
    , repo :: String
    , version :: Version
    , elmVersion :: Constraint
    , summary :: String
    , license :: String
    , sourceDirs :: Array FilePath
    , exposed :: Array Raw
    , natives :: Boolean
    , dependencies :: Array ( Tuple Name Constraint )
    }


derive instance newtypeDescription :: Newtype Description _


instance foreignableDescription :: Foreignable Description where
  put (Description value) =
    Foreign.toForeign <|
      { name: put value.name
      , repository: put value.repo
      , version: put value.version
      , "elm-version": put value.elmVersion
      , summary: put value.summary
      , license: put value.license
      , "source-directories": put value.sourceDirs
      , "exposed-modules": put value.exposed
      , "native-modules": put value.natives
      , dependencies: putDependencies value.dependencies
      }

  get value =
    { name: _, repo: _, version: _, elmVersion: _, summary: _
    , license: _, sourceDirs: _, exposed: _, natives: _
    , dependencies: _
    }
      <$> (value ! "repository" >>= getName)
      <*> (value ! "repository" >>= get)
      <*> (value ! "version" >>= get)
      <*> (value ! "elm-version" >>= get)
      <*> (value ! "summary" >>= get)
      <*> (value ! "license" >>= get)
      <*> (value ! "source-directories" >>= get)
      <*> (value ! "exposed-modules" >>= get)
      <*> (value ! "native-modules" >>= Foreign.readUndefined >>= traverse get <#> Maybe.fromMaybe false)
      <*> (value ! "dependencies" >>= getDependencies)
      <#> Description


getName :: Foreign -> F Name
getName value = do
  string <- get value
  case String.split (Pattern "github.com/") string of
    [ _, name ] ->
      name
        |> String.replace (Pattern ".git") (Replacement "")
        |> read
        |> lmap (Foreign.ForeignError >>> NonEmptyList.singleton)
        |> except

    _ ->
      Foreign.fail (Foreign.ForeignError "Bad repo")

getDependencies :: Foreign -> F (Array ( Tuple Name Constraint ))
getDependencies object =
  object
    |> Foreign.unsafeReadTagged "Object"
    <#> StrMap.toUnfoldable
    >>= traverse 
        (\(Tuple k v) ->
            Tuple
              <$> read k
              <*> read v
              |> lmap (Foreign.ForeignError >>> NonEmptyList.singleton)
              |> except
        )


putDependencies :: Array ( Tuple Name Constraint ) -> Foreign
putDependencies values =
  values
    |> map (bimap show show)
    |> StrMap.fromFoldable
    |> Foreign.toForeign



filePath :: Name -> Version -> FilePath
filePath name version =
  "elm-stuff/packages"
    </> show name
    </> show version
    </> "elm-package.json"


isSaved :: ∀ e x. Name -> Version -> Task (fileSystem :: FILESYSTEM | e) x Boolean
isSaved name version =
  FileSystem.exists <| filePath name version


save :: ∀ e x. Name -> Version -> Description -> Task (fileSystem :: FILESYSTEM | e) x Unit
save name version description =
  FileSystem.write (filePath name version) description


load :: ∀ e. Name -> Version -> Task (fileSystem :: FILESYSTEM | e) FileSystem.Error Description
load name version =
  FileSystem.read (filePath name version)


fetch :: ∀ e. Name -> Version -> Task (http :: HTTP | e) Http.Error Description
fetch (Name name) version =
  Url.crossOrigin Constants.cdnBase [ "package-artifacts", name.user, name.project, show version, "elm-package.json" ] []
    |> Http.get
    |> Http.withExpect Http.expectJson
    |> Http.send
