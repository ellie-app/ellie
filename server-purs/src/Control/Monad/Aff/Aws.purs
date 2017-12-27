module Control.Monad.Aff.Aws
  ( AWS
  , Client
  , ClientOptions
  , GetObjectOptions
  , PutObjectOptions
  , CreatePresignedPostOptions
  , createClient
  , putObject
  , getObject
  , headObject
  , createPresignedPost
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat as Aff
import Control.Monad.Eff (kind Effect)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.StrMap (StrMap)

foreign import data AWS :: Effect
foreign import data Client :: Type
foreign import _createClient :: ∀ e. ClientOptions -> EffFnAff (aws :: AWS | e) Client
foreign import _putObject :: ∀ e. Fn3 Unit Client PutObjectOptions (EffFnAff (aws :: AWS | e) Unit)
foreign import _getObject :: ∀ e. Fn2 Client GetObjectOptions (EffFnAff (aws :: AWS | e) String)
foreign import _headObject :: ∀ e. Fn3 Unit Client GetObjectOptions (EffFnAff (aws :: AWS | e) Unit)
foreign import _createPresignedPost :: ∀ e. Fn2 Client CreatePresignedPostOptions (EffFnAff (aws :: AWS | e) (StrMap String))


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


type CreatePresignedPostOptions =
  { key :: String
  , bucket :: String
  , fields :: Array { key :: String, value :: String }
  }


createClient :: ∀ e. ClientOptions -> Aff (aws :: AWS | e) Client
createClient options = Aff.fromEffFnAff $ _createClient options


putObject :: ∀ e. Client -> PutObjectOptions -> Aff (aws :: AWS | e) Unit
putObject client options = Aff.fromEffFnAff $ runFn3 _putObject unit client options


getObject :: ∀ e. Client -> GetObjectOptions -> Aff (aws :: AWS | e) String
getObject client options = Aff.fromEffFnAff $ runFn2 _getObject client options


headObject :: ∀ e. Client -> GetObjectOptions -> Aff (aws :: AWS | e) Unit
headObject client options = Aff.fromEffFnAff $ runFn3 _headObject unit client options


createPresignedPost :: ∀ e. Client -> CreatePresignedPostOptions -> Aff (aws :: AWS | e) (StrMap String)
createPresignedPost client options = Aff.fromEffFnAff $ runFn2 _createPresignedPost client options