module Data.String.Class
  ( class ToString
  , toString
  , class FromString
  , fromString
  ) where

import Prelude
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number

class ToString a where
  toString ∷ a → String

instance toStringString ∷ ToString String where
  toString = id

instance toStringInt ∷ ToString Int where
  toString = show

instance toStringNumber ∷ ToString Number where
  toString = show

instance toStringBoolean ∷ ToString Boolean where
  toString = show


class FromString a where
  fromString ∷ String → Maybe a

instance fromStringString ∷ FromString String where
  fromString = Just

instance fromStringInt ∷ FromString Int where
  fromString = Int.fromString

instance fromStringNumber ∷ FromString Number where
  fromString = Number.fromString

instance fromStringBoolean ∷ FromString Boolean where
  fromString "true" = Just true
  fromString "false" = Just false
  fromString _ = Nothing