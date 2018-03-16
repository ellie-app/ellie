module System.Postgres
  ( class ToValue
  , toValue
  , Arguments
  , arguments
  , class ToArguments
  , toArguments
  , class FromResult
  , fromResult
  , invoke
  , Invocation
  , exec
  , Client
  , connect
  ) where


import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO (IO)
import Control.Monad.IO.Effect (INFINITY)
import Data.Either (Either(..))
import Data.Entity (class IdentifiedBy, Entity)
import Data.Entity as Entity
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.String.Class as String
import Data.Uuid (Uuid)
import Data.Uuid as Uuid
import Debug as Debug


foreign import data Client ∷ Type
foreign import _exec ∷ Fn3 Client String Json (EffFnAff (infinity ∷ INFINITY) Json)
foreign import _connect ∷ String → EffFnAff (infinity ∷ INFINITY) Client


newtype Invocation =
  Invocation
    { name ∷ String
    , inputs ∷ Json
    }


class ToValue a where
  toValue ∷ a → Json

instance toValueUuid ∷ ToValue Uuid where toValue = String.toString >>> Json.encodeString
instance toValueJson ∷ ToValue Json where toValue = id
instance toValueInt ∷ ToValue Int where toValue = Json.encodeInt
instance toValueNumber ∷ ToValue Number where toValue = Json.encodeNumber
instance toValueString ∷ ToValue String where toValue = Json.encodeString
instance toValueBoolean ∷ ToValue Boolean where toValue = Json.encodeBoolean
instance toValueArray ∷ ToValue a ⇒ ToValue (Array a) where toValue = Json.encodeArray toValue
instance toValueMaybe ∷ ToValue a ⇒ ToValue (Maybe a) where
  toValue (Just a) = toValue a
  toValue Nothing = Json.encodeNull


class ToArguments a where
  toArguments ∷ a → Array { key ∷ String, value ∷ Json }

newtype Arguments =
  Arguments (Array { key ∷ String, value ∷ Json })

arguments ∷ Array { key ∷ String, value ∷ Json } → Arguments
arguments = Arguments

instance toArgumentsArguments ∷ ToArguments Arguments where
  toArguments (Arguments stuff) = stuff

instance toArgumentsEntity ∷ (ToArguments k, ToArguments r, IdentifiedBy k r) ⇒ ToArguments (Entity k r) where
  toArguments entity =
    toArguments (Entity.key entity) <> toArguments (Entity.record entity)

class FromResult a where
  fromResult ∷ Json → Either Json.Error a

instance fromResultUnit ∷ FromResult Unit where fromResult _ = Right unit
instance fromResultInt ∷ FromResult Int where fromResult = Json.decodeInt
instance fromResultNumber ∷ FromResult Number where fromResult = Json.decodeNumber
instance fromResultString ∷ FromResult String where fromResult = Json.decodeString
instance fromResultBoolean ∷ FromResult Boolean where fromResult = Json.decodeBoolean
instance fromResultArray ∷ FromResult a ⇒ FromResult (Array a) where fromResult = Json.decodeArray fromResult

instance fromResultUuid ∷ FromResult Uuid where
  fromResult v =
    Json.decodeString v >>= \s →
      case Uuid.fromString s of
        Just uuid → Right uuid
        Nothing → Left $ Json.Failure "Expecting a UUID" v

instance fromResultMaybe ∷ FromResult a ⇒ FromResult (Maybe a) where
  fromResult v =
    (Just <$> fromResult v) <|> (Json.decodeNull Nothing v)

instance fromResultEntity ∷ (FromResult k, FromResult r, IdentifiedBy k r) ⇒ FromResult (Entity k r) where
  fromResult a =
    Entity.entity
      <$> fromResult a
      <*> fromResult a

invoke ∷ ∀ a. ToArguments a ⇒ String → a → Invocation
invoke name inputs =
  Invocation { name, inputs: Json.encodeObject $ toArguments inputs }


exec ∷ ∀ a. FromResult a ⇒ Client → Invocation → IO a
exec client (Invocation i) = do
  value ← Aff.liftAff $ Aff.fromEffFnAff $ runFn3 _exec client i.name i.inputs
  let either = fromResult value
  case either of
    Left message → throwError $ error $ String.toString message
    Right parsed → pure $ parsed


connect ∷ String → IO Client
connect connection =
  Aff.liftAff $ Aff.fromEffFnAff $ _connect connection
