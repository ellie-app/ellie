module System.Postgres
  ( Client
  , connect
  , invoke
  , Invocation
  , exec
  ) where


import Prelude

import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO (IO)
import Control.Monad.IO.Effect (INFINITY)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Json (Json)
import Data.Json as Json


foreign import data Client ∷ Type
foreign import _exec ∷ Fn3 Client String Json (EffFnAff (infinity ∷ INFINITY) Json)
foreign import _connect ∷ String → EffFnAff (infinity ∷ INFINITY) Client


newtype Invocation =
  Invocation
    { name ∷ String
    , inputs ∷ Json
    }


invoke ∷ String → Array Json.KeyValue → Invocation
invoke name inputs =
  Invocation { name, inputs: Json.encodeObject $ inputs }


exec ∷ ∀ a. Client → (Json → Either Json.Error a) → Invocation → IO a
exec client decode (Invocation i) = do
  value ← Aff.liftAff $ Aff.fromEffFnAff $ runFn3 _exec client i.name i.inputs
  let either = decode value
  case either of
    Left message → throwError $ error $ Json.errorToString message
    Right parsed → pure $ parsed


connect ∷ String → IO Client
connect connection =
  Aff.liftAff $ Aff.fromEffFnAff $ _connect connection
