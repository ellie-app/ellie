module Data.Uuid
  ( Uuid
  , fromString
  , encode
  , decode
  , zero
  ) where

import Prelude

import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode)
import Data.Maybe (Maybe(..))
import Data.String.Class (class ToString)


foreign import _encode ∷ String → String
foreign import _decode ∷ { just ∷ ∀ a. a → Maybe a, nothing ∷ ∀ a. Maybe a, string ∷ String } → Maybe String
foreign import _validate ∷ String → Boolean


newtype Uuid =
  Uuid String

derive newtype instance eqUuid ∷ Eq Uuid
derive newtype instance ordUuid ∷ Ord Uuid

instance decodeUuid ∷ Decode Uuid where
  decode value = do
    string ← Foreign.readString value
    case fromString string of
      Just uuid → pure uuid
      Nothing → Foreign.fail (Foreign.ForeignError "Expecting a UUID string in the format 00112233-4455-V677-8899-aabbccddeeff")

instance toStringUuid ∷ ToString Uuid where
  toString (Uuid string) = string


fromString ∷ String → Maybe Uuid
fromString input =
  if _validate input then
    Just $ Uuid input
  else
    Nothing


encode ∷ Uuid → String
encode (Uuid inner) = _encode inner


decode ∷ String → Maybe Uuid
decode string =
  Uuid <$> _decode { just: Just, nothing: Nothing, string }


zero ∷ Uuid
zero =
  Uuid "00000000-0000-0000-0000-000000000000"
