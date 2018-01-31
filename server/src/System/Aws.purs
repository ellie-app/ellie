module System.Aws
  ( Error
  , Client
  , ClientOptions
  , GetObjectOptions
  , PutObjectOptions
  , ListObjectsOptions
  , createClient
  , putObject
  , getObject
  , headObject
  , listObjects
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat as Aff
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class as Eff
import Control.Monad.IO (IO)
import Data.Either (Either(..))
import Data.StrMap (StrMap)


foreign import data AWS :: Effect
foreign import data Client :: Type
foreign import _createClient :: ∀ e. ClientOptions -> Eff (aws :: AWS | e) Client
foreign import _putObject :: ∀ e. { helpers :: FfiHelpers, client :: Client, options :: PutObjectOptions } -> EffFnAff (aws :: AWS | e) (Either Error Unit)
foreign import _getObject :: ∀ e. { helpers :: FfiHelpers, client :: Client, options :: GetObjectOptions } -> EffFnAff (aws :: AWS | e) (Either Error String)
foreign import _headObject :: ∀ e. { helpers :: FfiHelpers, client :: Client, options :: GetObjectOptions } -> EffFnAff (aws :: AWS | e) (Either Error Unit)
foreign import _listObjects :: ∀ e. { helpers :: FfiHelpers, client :: Client, options :: ListObjectsOptions } -> EffFnAff (aws :: AWS | e) (Either Error (Array String))


type FfiHelpers =
  { unit :: Unit
  , left :: ∀ x a. x -> Either x a
  , right :: ∀ x a. a -> Either x a
  }


helpers :: FfiHelpers
helpers =
  { unit: unit
  , left: Left
  , right: Right
  }


type Error =
  { code :: String
  , message :: String
  , retryable :: Boolean
  , statusCode :: Int
  , hostname :: String
  , region :: String
  }


type ClientOptions =
  { accessKeyId :: String
  , secretAccessKey :: String
  }


type PutObjectOptions =
  { key :: String
  , bucket :: String
  , contents :: String
  , mimeType :: String
  }


type GetObjectOptions =
  { key :: String
  , bucket :: String
  }


type ListObjectsOptions =
  { prefix :: String
  , bucket :: String
  }


createClient :: ClientOptions -> IO Client
createClient = Eff.liftEff <<< _createClient


putObject :: Client -> PutObjectOptions -> IO (Either Error Unit)
putObject client options =
  Aff.liftAff $ Aff.fromEffFnAff $ _putObject { helpers, client, options }


getObject :: Client -> GetObjectOptions -> IO (Either Error String)
getObject client options =
  Aff.liftAff $ Aff.fromEffFnAff $ _getObject { helpers, client, options }


headObject :: Client -> GetObjectOptions -> IO (Either Error Unit)
headObject client options =
  Aff.liftAff $ Aff.fromEffFnAff $ _headObject { helpers, client, options }


listObjects :: Client -> ListObjectsOptions -> IO (Either Error (Array String))
listObjects client options =
  Aff.liftAff $ Aff.fromEffFnAff $ _listObjects { helpers, client, options }