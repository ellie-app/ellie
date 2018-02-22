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
import Control.Monad.Except as Except
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class (liftIO) as IO
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (either) as Either
import Data.Foreign (Foreign, F)
import Data.Foreign (readArray, renderForeignError) as Foreign
import Data.Foreign.Class (decode) as Foreign
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String as String
import Data.String.Class (toString) as String
import Data.Traversable (maximum, traverse)
import Debug as Debug
import Elm.Package (Package(..))
import Elm.Package.Name (Name)
import Elm.Package.Searchable (Searchable(..))
import Elm.Package.Version (Version)
import Elm.Package.Version as Version
import System.Http as Http
import System.SearchIndex (SearchIndex)
import System.SearchIndex as SearchIndex


type Env r =
  { index ∷ Ref (Maybe (SearchIndex Searchable))
  | r
  }


search ∷ ∀ m r. MonadIO m ⇒ MonadThrow Error m => String → Env r -> m (Array Searchable)
search query env = IO.liftIO do
  maybeIndex ←
    Eff.liftEff $ Ref.readRef env.index
  index ←
    case maybeIndex of
      Just i → pure i
      Nothing -> do
        rawPackageData ←
          Http.get "http://package.elm-lang.org/all-packages"
        rawNewPackageNames ←
          Http.get "http://package.elm-lang.org/new-packages"
        (newPackageNamesArray ∷ Array Name) ←
          rawNewPackageNames
            # Foreign.decode
            # Except.runExcept
            # lmap (\me → error $ String.joinWith "\n" $ Array.fromFoldable $ map Foreign.renderForeignError me)
            # Either.either throwError pure
        let newPackageNames = Set.fromFoldable newPackageNamesArray
        nameAndVersions ←
          rawPackageData
            # decodeNameAndVersions
            # Except.runExcept
            # lmap (\me → error $ String.joinWith "\n" $ Array.fromFoldable $ map Foreign.renderForeignError me)
            # Either.either throwError pure
        newIndex ←
          SearchIndex.create
        nameAndVersions
          # map toSearchable
          # Array.filter (\(Searchable s) → Set.member (s.package # unwrap # _.name) newPackageNames)
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

    decodeNameAndVersion ∷ Foreign → F { description ∷ String, name ∷ Name, versions ∷ Array Version }
    decodeNameAndVersion object = do
      { name: _, versions: _, description: _ }
        <$> (object ! "name" >>= Foreign.decode)
        <*> (object ! "versions" >>= Foreign.decode)
        <*> (object ! "summary" >>= Foreign.decode)

    decodeNameAndVersions ∷ Foreign → F (Array { description ∷ String, name :: Name, versions :: Array Version })
    decodeNameAndVersions array =
      Foreign.readArray array >>= traverse decodeNameAndVersion
