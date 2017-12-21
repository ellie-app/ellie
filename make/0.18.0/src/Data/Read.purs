module Data.Read
  ( class Read
  , read
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Either as Either
import Data.Int as Int
import Data.Number as Number
import Data.String as String

class Read a where
  read :: String -> Either String a

instance readString :: Read String where
  read = Right

instance readChar :: Read Char where
  read = String.charAt 0 >>> Either.note "Expecting a non-empty String"

instance readBoolean :: Read Boolean where
  read s =
    case s of
      "true"  -> Right true
      "false" -> Right false
      _       -> Left "Expecting `\"true\"` or `\"false\"`"

instance readInt :: Read Int where
  read = Int.fromString >>> Either.note "Expecting an Int"

instance readNumber :: Read Number where
  read = Number.fromString >>> Either.note "Expecting a Number"
