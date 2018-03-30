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
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Time.Good (Posix)
import Data.Time.Good as Time
import Ellie.Adapters.CdnAssets as CdnAssets
import Ellie.Adapters.DatabaseRepo as DatabaseRepo
import Ellie.Adapters.DatabaseSearch as DatabaseSearch
import Ellie.Adapters.LocalPlatform as LocalPlatform
import Ellie.Adapters.WebpackAssets as WebpackAssets
import Ellie.Domain.Assets (class Assets)
import Ellie.Domain.Platform (class Platform)
import Ellie.Domain.RevisionRepo (class RevisionRepo)
import Ellie.Domain.Search (class Search)
import Ellie.Domain.UserRepo (class UserRepo)
import Ellie.Types.User as User
import Elm.Package (Package)
import System.Aws as Aws
import System.Cache (Cache)
import System.Jwt (Secret)
import System.Postgres as Postgres
import System.Redis as Redis
import Type.Equality (class TypeEquals, to)


type SharedEnv r =
  { jwtSecret ∷ Secret
  , assetBase ∷ String
  , defaultPackages ∷ Cache (Array Package)
  , userWorkspaces ∷ Ref (Map User.Id LocalPlatform.Workspace)
  , packageSite ∷ String
  , postgresClient ∷ Postgres.Client
  , lastSearchUpdate ∷ Ref (Maybe Posix)
  , searchRefresh ∷ Time.Span
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

derive instance newtypeProdEnv ∷ Newtype ProdEnv _


newtype DevEnv =
  DevEnv
    (SharedEnv
      ( webpackHost ∷ String
      )
    )

derive instance newtypeDevEnv ∷ Newtype DevEnv _


newtype EllieM r a =
  EllieM (ReaderT r IO a)

derive instance newtypeAppM ∷ Newtype (EllieM r a) _
derive newtype instance functorAppM ∷ Functor (EllieM r)
derive newtype instance applyAppM ∷ Apply (EllieM r)
derive newtype instance applicativeAppM ∷ Applicative (EllieM r)
derive newtype instance bindAppM ∷ Bind (EllieM r)
derive newtype instance monadAppM ∷ Monad (EllieM r)
derive newtype instance monadIOAppM ∷ MonadIO (EllieM r)
derive newtype instance monadAskAppM ∷ MonadAsk r (EllieM r)
derive newtype instance monadReaderAppM ∷ MonadReader r (EllieM r)
derive newtype instance monadThrowAppM ∷ MonadThrow Error (EllieM r)
derive newtype instance monadErrorAppM ∷ MonadError Error (EllieM r)

instance revisionRepoEllieMProdEnv ∷ (Newtype e a, TypeEquals a (SharedEnv e0)) ⇒ RevisionRepo (EllieM e) where
  retrieve rid = Reader.asks (unwrap >>> to) >>= DatabaseRepo.getRevision rid
  exists rid = Reader.asks (unwrap >>> to) >>= DatabaseRepo.revisionExists rid
  create revision = Reader.asks (unwrap >>> to) >>= DatabaseRepo.createRevision revision
  update entity = Reader.asks (unwrap >>> to) >>= DatabaseRepo.updateRevision entity

instance userRepoEllieM ∷ (Newtype e a, TypeEquals a (SharedEnv e0)) ⇒ UserRepo (EllieM e) where
  retrieve uid = Reader.asks (unwrap >>> to) >>= DatabaseRepo.getUser uid
  create = Reader.asks (unwrap >>> to) >>= DatabaseRepo.createUser
  save uid user = Reader.asks (unwrap >>> to) >>= DatabaseRepo.saveUser uid user
  verify jwt = Reader.asks (unwrap >>> to) >>= DatabaseRepo.verifyUser jwt
  sign uid = Reader.asks (unwrap >>> to) >>= DatabaseRepo.signUser uid

instance searchEllieM ∷ (Newtype e a, TypeEquals a (SharedEnv e0)) ⇒ Search (EllieM e) where
  search query = Reader.asks (unwrap >>> to) >>= DatabaseSearch.search query

instance platformEllieM ∷ (Newtype e a, TypeEquals a (SharedEnv e0)) ⇒ Platform (EllieM e) where
  initialize userId = Reader.asks (unwrap >>> to) >>= LocalPlatform.initialize userId
  destroy userId = Reader.asks (unwrap >>> to) >>= LocalPlatform.destroy userId
  compile elm html packages userId = Reader.asks (unwrap >>> to) >>= LocalPlatform.compile elm html packages userId
  format = LocalPlatform.format
  result userId = Reader.asks (unwrap >>> to) >>= LocalPlatform.result userId

instance assetsEllieMProdEnv ∷ Assets (EllieM ProdEnv) where
  assetUrl relative = Reader.asks unwrap <#> CdnAssets.assetUrl relative
  termsHtml = CdnAssets.termsHtml

instance assetsEllieMDevEnv ∷ Assets (EllieM DevEnv) where
  assetUrl relative = Reader.asks unwrap <#> WebpackAssets.assetUrl relative
  termsHtml = WebpackAssets.termsHtml


runEllieM ∷ ∀ r a. r → EllieM r a → IO a
runEllieM env (EllieM reader) =
  Reader.runReaderT reader env


class (Monad m, UserRepo m, RevisionRepo m, Search m, Platform m, Assets m) ⇐ Ellie m
instance ellieEllieMDevEnv ∷ Ellie (EllieM DevEnv)
instance ellieEllieMProdEnv ∷ Ellie (EllieM ProdEnv)
