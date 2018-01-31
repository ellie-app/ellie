module System.Redis
  ( Options
  , Client
  , defaults
  , create
  , get
  , set
  , exists
  ) where


import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat as Aff
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class as Eff
import Control.Monad.IO (IO)
import Data.Foreign (Foreign)
import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (encode, decode) as Foreign
import Data.Foreign.NullOrUndefined (NullOrUndefined(..)) as Foreign
import Data.Maybe (Maybe(..))

foreign import data REDIS :: Effect
foreign import data Client :: Type
foreign import _createClient :: ∀ e. Foreign -> Eff (redis :: REDIS | e) Client
foreign import _get :: ∀ e. { just :: ∀ a. a -> Maybe a, nothing :: ∀ a. Maybe a, client :: Client, key :: String } -> EffFnAff (redis :: REDIS | e) (Maybe Foreign)
foreign import _set :: ∀ e. { client :: Client, key :: String, value :: Foreign, unit :: Unit } -> EffFnAff (redis :: REDIS | e) Unit
foreign import _exists :: ∀ e. { client :: Client, key :: String } -> EffFnAff (redis :: REDIS | e) Boolean


type Options =
  { host :: String
	, port :: Int
	, maxRetries :: Int
	, auth :: Maybe String
	, db :: Maybe Int
	, autoConnect :: Boolean
  }


defaults :: Options
defaults =
  {	host: "127.0.0.1"
  , port: 6379
  , maxRetries: 10
  , auth: Nothing
	, db: Nothing
	, autoConnect: true
  }


encodeOptions :: Options -> Foreign
encodeOptions options =
  Foreign.toForeign
    { host: Foreign.encode options.host
    , port: Foreign.encode options.port
    , maxRetries: Foreign.encode options.maxRetries
    , auth: Foreign.encode $ Foreign.NullOrUndefined options.auth
    , db: Foreign.encode $ Foreign.NullOrUndefined options.db
    , autoConnect: Foreign.encode options.autoConnect
    }


create :: Options -> IO Client
create options =
  Eff.liftEff $ _createClient $ encodeOptions options


get :: Client -> String -> IO (Maybe Foreign)
get client key =
  Aff.liftAff $ Aff.fromEffFnAff $ _get { just: Just, nothing: Nothing, client, key }


set :: Client -> String -> Foreign -> IO Unit
set client key value =
  Aff.liftAff $ Aff.fromEffFnAff $ _set { client, key, value, unit }


exists :: Client -> String -> IO Boolean
exists client key =
  Aff.liftAff $ Aff.fromEffFnAff $ _exists { client, key }