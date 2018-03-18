module Data.Uuid
  ( Uuid
  , toString
  , fromString
  , base49Encode
  , base49Decode
  , zero
  ) where

import Prelude

import Data.Maybe (Maybe(..))


foreign import _encode ∷ String → String
foreign import _decode ∷ { just ∷ ∀ a. a → Maybe a, nothing ∷ ∀ a. Maybe a, string ∷ String } → Maybe String
foreign import _validate ∷ String → Boolean


newtype Uuid =
  Uuid String

derive newtype instance eqUuid ∷ Eq Uuid
derive newtype instance ordUuid ∷ Ord Uuid


toString ∷ Uuid → String
toString (Uuid string) = string


fromString ∷ String → Maybe Uuid
fromString input =
  if _validate input then
    Just $ Uuid input
  else
    Nothing


base49Encode ∷ Uuid → String
base49Encode (Uuid inner) = _encode inner


base49Decode ∷ String → Maybe Uuid
base49Decode string =
  Uuid <$> _decode { just: Just, nothing: Nothing, string }


zero ∷ Uuid
zero =
  Uuid "00000000-0000-0000-0000-000000000000"
