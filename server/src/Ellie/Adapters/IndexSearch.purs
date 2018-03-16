module Ellie.Adapters.IndexSearch
  ( Env
  , search
  ) where

import Prelude

import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class (liftIO) as IO
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Either (either) as Either
import Data.Json (Json)
import Data.Json (Error, decodeArray, decodeAtField) as Json
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String.Class (toString) as String
import Data.Traversable (maximum)
import Elm.Name (Name)
import Elm.Package (Package(..))
import Elm.Package.Searchable (Searchable(..))
import Elm.Version (Version)
import Elm.Version as Version
import System.Http as Http
import System.SearchIndex (SearchIndex)
import System.SearchIndex as SearchIndex
import Server.Action as Action


type Env r =
  { index ∷ Ref (Maybe (SearchIndex Searchable))
  , packageSite ∷ String
  | r
  }


search ∷ ∀ m r. MonadIO m ⇒ MonadThrow Error m ⇒ String → Env r → m (Array Searchable)
search query env = IO.liftIO do
  maybeIndex ←
    Eff.liftEff $ Ref.readRef env.index
  index ←
    case maybeIndex of
      Just i → pure i
      Nothing → do
        rawPackageData ←
          Http.get $ env.packageSite <> "/search.json"
        nameAndVersions ←
          rawPackageData
            # decodeNameAndVersions
            # lmap (String.toString >>> error)
            # Either.either throwError pure
        newIndex ←
          SearchIndex.create
        nameAndVersions
          # map toSearchable
          # SearchIndex.add newIndex
        Eff.liftEff $ Ref.writeRef env.index (Just newIndex)
        pure newIndex
  SearchIndex.search index query 20
  where
    toSearchable ∷ { description ∷ String, name ∷ Name, versions ∷ Array Version } → Searchable
    toSearchable { name, description, versions } =
      Searchable
        { package:
            Package
              { name: name
              , version: Maybe.fromMaybe Version.zero $ maximum versions
              }
        , description
        }

    decodeNameAndVersion ∷ Json → Either Json.Error { description ∷ String, name ∷ Name, versions ∷ Array Version }
    decodeNameAndVersion value = do
      { name: _, versions: _, description: _ }
        <$> Json.decodeAtField "name" value Action.fromBody
        <*> Json.decodeAtField "versions" value Action.fromBody
        <*> Json.decodeAtField "summary" value Action.fromBody

    decodeNameAndVersions ∷ Json → Either Json.Error (Array { description ∷ String, name ∷ Name, versions ∷ Array Version })
    decodeNameAndVersions array =
      Json.decodeArray decodeNameAndVersion array
