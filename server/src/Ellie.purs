module Ellie
  ( class Ellie
  , EllieM
  , DevEnv(..)
  , ProdEnv(..)
  , SharedEnv
  , runEllieM
  ) where

import Prelude
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError)
import Control.Monad.IO (IO)
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.Reader (ReaderT, class MonadReader, class MonadAsk)
import Control.Monad.Reader as Reader
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Ellie.Adapters.CloudRepo as CloudRepo
import Ellie.Adapters.IndexSearch as IndexSearch
import Ellie.Adapters.LocalPlatform as LocalPlatform
import Ellie.Adapters.LocalRepo as LocalRepo
import Ellie.Adapters.WebpackAssets as WebpackAssets
import Ellie.Adapters.CdnAssets as CdnAssets
import Ellie.Domain.Platform (class Platform)
import Ellie.Domain.RevisionRepo (class RevisionRepo)
import Ellie.Domain.Search (class Search)
import Ellie.Domain.UserRepo (class UserRepo)
import Ellie.Domain.Assets (class Assets)
import Ellie.Elm.Package (Package)
import System.Aws as Aws
import System.Jwt (Secret)
import System.Redis as Redis
import System.SearchIndex (SearchIndex)
import Type.Equality (class TypeEquals, to)


type SharedEnv r =
  { index ∷ Ref (Maybe (SearchIndex Package))
  , jwtSecret ∷ Secret
  , assetBase ∷ String
  | r
  }


newtype ProdEnv =
  ProdEnv
    (SharedEnv 
      ( redisClient ∷ Redis.Client
      , awsClient ∷ Aws.Client
      , bucket ∷ String
      , cdnHost ∷ String
      )
    )

derive instance newtypeProdEnv :: Newtype ProdEnv _


newtype DevEnv =
  DevEnv
    (SharedEnv
      ( webpackHost ∷ String
      )
    )

derive instance newtypeDevEnv :: Newtype DevEnv _


newtype EllieM r a =
  EllieM (ReaderT r IO a)

derive instance newtypeAppM :: Newtype (EllieM r a) _
derive newtype instance functorAppM :: Functor (EllieM r)
derive newtype instance applyAppM :: Apply (EllieM r)
derive newtype instance applicativeAppM :: Applicative (EllieM r)
derive newtype instance bindAppM :: Bind (EllieM r)
derive newtype instance monadAppM :: Monad (EllieM r)
derive newtype instance monadIOAppM :: MonadIO (EllieM r)
derive newtype instance monadAskAppM :: MonadAsk r (EllieM r)
derive newtype instance monadReaderAppM :: MonadReader r (EllieM r)
derive newtype instance monadThrowAppM :: MonadThrow Error (EllieM r)
derive newtype instance monadErrorAppM :: MonadError Error (EllieM r)

instance revisionRepoEllieMProdEnv ∷ RevisionRepo (EllieM ProdEnv) where
  retrieve rid = Reader.asks unwrap >>= CloudRepo.getRevision rid
  exists rid = Reader.asks unwrap >>= CloudRepo.revisionExists rid
  save rid revision = Reader.asks unwrap >>= CloudRepo.saveRevision rid revision

instance revisionRepoEllieMDevEnv ∷ RevisionRepo (EllieM DevEnv) where
  retrieve rid = LocalRepo.getRevision rid
  exists rid = LocalRepo.revisionExists rid
  save rid revision = LocalRepo.saveRevision rid revision

instance userRepoEllieMProdEnv ∷ UserRepo (EllieM ProdEnv) where
  retrieve uid = Reader.asks unwrap >>= CloudRepo.getUser uid
  create = Reader.asks unwrap >>= CloudRepo.createUser
  save uid user = Reader.asks unwrap >>= CloudRepo.saveUser uid user
  verify jwt = Reader.asks unwrap >>= CloudRepo.verifyUser jwt
  sign uid = Reader.asks unwrap >>= CloudRepo.signUser uid

instance userRepoEllieMDevEnv ∷ UserRepo (EllieM DevEnv) where
  retrieve uid = LocalRepo.getUser uid
  create = LocalRepo.createUser
  save uid user = LocalRepo.saveUser uid user
  verify jwt = Reader.asks unwrap >>= LocalRepo.verifyUser jwt
  sign uid = Reader.asks unwrap >>= LocalRepo.signUser uid

instance searchEllieM ∷ (Newtype e a, TypeEquals a (SharedEnv e0)) ⇒ Search (EllieM e) where
  search query = Reader.asks (unwrap >>> to) >>= IndexSearch.search query

instance platformEllieM ∷ Platform (EllieM e) where
  initialize = LocalPlatform.initialize
  destroy = LocalPlatform.destroy
  compile = LocalPlatform.compile

instance assetsEllieMProdEnv ∷ Assets (EllieM ProdEnv) where
  assetUrl relative = Reader.asks unwrap <#> CdnAssets.assetUrl relative

instance assetsEllieMDevEnv ∷ Assets (EllieM DevEnv) where
  assetUrl relative = Reader.asks unwrap <#> WebpackAssets.assetUrl relative


runEllieM :: ∀ r a. r -> EllieM r a -> IO a
runEllieM env (EllieM reader) =
  Reader.runReaderT reader env


class (Monad m, UserRepo m, RevisionRepo m, Search m, Assets m) ⇐ Ellie m
instance ellieEllieMDevEnv ∷ Ellie (EllieM DevEnv)
instance ellieEllieMProdEnv ∷ Ellie (EllieM ProdEnv)
