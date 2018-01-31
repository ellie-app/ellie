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
import Control.Monad.IO.Class (liftIO) as IO
import Control.Monad.IO.Class (class MonadIO)
import System.Http as Http
import System.SearchIndex (SearchIndex)
import System.SearchIndex as SearchIndex
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (either) as Either
import Data.Foreign (Foreign, F)
import Data.Foreign (readArray, renderForeignError) as Foreign
import Data.Foreign.Class (decode) as Foreign
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Ellie.Elm.Package (Package(..))
import Ellie.Elm.Package.Name (Name)
import Ellie.Elm.Package.Version (Version)


type Env r =
  { index :: Ref (Maybe (SearchIndex Package))
  | r
  }


search :: ∀ m r. MonadIO m => MonadThrow Error m => String -> Env r -> m (Array Package)
search query env = do
  maybeIndex <-
    IO.liftIO $ Eff.liftEff $ Ref.readRef env.index
  index <-
    case maybeIndex of
      Just i -> pure i
      Nothing -> do
        rawPackageData <-
          IO.liftIO $ Http.get "https://package.elm-lang.org/all-packages.json"
        nameAndVersions <-
          rawPackageData
            # decodeNameAndVersions
            # Except.runExcept
            # lmap (\me -> error $ String.joinWith "\n" $ Array.fromFoldable $ map Foreign.renderForeignError me)
            # Either.either throwError pure
        newIndex <-
          IO.liftIO $ SearchIndex.create
        nameAndVersions
          # Array.concatMap toPackages
          # SearchIndex.add newIndex
          # IO.liftIO
        IO.liftIO $ Eff.liftEff $ Ref.writeRef env.index (Just newIndex)
        pure newIndex
  IO.liftIO $ SearchIndex.search index query 5
  where
    toPackages :: { name :: Name, versions :: Array Version } -> Array Package
    toPackages { name, versions } =
      map (\version -> Package { name, version }) versions

    decodeNameAndVersion :: Foreign -> F { name :: Name, versions :: Array Version }
    decodeNameAndVersion object = do
      { name: _, versions: _ }
        <$> (object ! "name" >>= Foreign.decode)
        <*> (object ! "versions" >>= Foreign.decode)

    decodeNameAndVersions :: Foreign -> F (Array { name :: Name, versions :: Array Version })
    decodeNameAndVersions array =
      Foreign.readArray array >>= traverse decodeNameAndVersion