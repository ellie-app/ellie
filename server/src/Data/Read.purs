module Data.Read where

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String

class Read a where
  read :: String -> Maybe a

instance readInt :: Read Int where
  read = Int.fromString

instance readNumber :: Read Number where
  read = Number.fromString

instance readString :: Read String where
  read = Just

instance readBoolean :: Read Boolean where
  read "true" = Just true
  read "false" = Just false
  read _ = Nothing

instance readChar :: Read Char where
  read input =
    case String.length input of
      1 -> String.charAt 0 input
      _ -> Nothing
