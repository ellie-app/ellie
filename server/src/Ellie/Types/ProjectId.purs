module Ellie.Types.ProjectId where

import Prelude

import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class as Foreign
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Class (class ToString)
import Server.Action (class IsParam)

newtype ProjectId =
  ProjectId
    { value :: BigInt
    , version :: Int
    }

instance showProjectId :: Show ProjectId where
  show (ProjectId { value }) = toStringV1 value

instance toStringProjectId ∷ ToString ProjectId where
  toString (ProjectId { value }) = toStringV1 value

instance encodeProjectId :: Encode ProjectId where
  encode pid = Foreign.encode (show pid)

instance decodeProjectId :: Decode ProjectId where
  decode = Foreign.decode >>> map fromString

instance isParamProjectId ∷ IsParam ProjectId where
  fromParam = fromString >>> Just


generate ::
  { releaseId :: Int
  , counter :: Int
  , now :: Int
  , epoch :: Int
  }
  -> ProjectId
generate { releaseId, counter, now, epoch } =
  zero
    # BigInt.orWith (BigInt.shiftLeftBy 23 (BigInt.fromInt now - BigInt.fromInt epoch))
    # BigInt.orWith (BigInt.shiftLeftBy 10 (BigInt.fromInt releaseId))
    # BigInt.orWith (BigInt.fromInt counter)
    # { value: _, version: 1 }
    # ProjectId


alphabet :: String
alphabet = "23456789bcdfghjkmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ"


baseLength :: Int
baseLength = String.length alphabet


baseLengthBigNum :: BigInt
baseLengthBigNum = BigInt.fromInt baseLength


fromString :: String -> ProjectId
fromString input =
  if String.contains (Pattern "a1") input then
    ProjectId { value: fromStringV1 input, version: 1 }
  else
    ProjectId { value: fromStringV0 input, version: 0 }


fromStringV0 :: String -> BigInt
fromStringV0 input =
  input
    # String.toCharArray
    # Array.foldl fold zero
  where
    fold :: BigInt -> Char -> BigInt
    fold tracker currentChar =
      currentChar
        # String.singleton
        # Pattern
        # (\p -> String.indexOf p alphabet)
        # Maybe.fromMaybe 0
        # (\i -> i + 1)
        # BigInt.fromInt
        # (\i -> tracker * baseLengthBigNum + i)


fromStringV1 :: String -> BigInt
fromStringV1 input =
  input
    # String.stripSuffix (Pattern "a1")
    # Maybe.fromMaybe ""
    # String.toCharArray
    # Array.foldl fold zero
  where
    fold :: BigInt -> Char -> BigInt
    fold tracker currentChar =
      currentChar
        # String.singleton
        # Pattern
        # (\p -> String.indexOf p alphabet)
        # Maybe.fromMaybe 0
        # BigInt.fromInt
        # (\i -> tracker * baseLengthBigNum + i)


toStringV1 :: BigInt -> String
toStringV1 value =
    go "" value <> "a1"
  where
    go string bigNum =
      if bigNum > zero then
        let
          index = BigInt.toInt $ (mod bigNum baseLengthBigNum) - one
          tracker = bigNum / baseLengthBigNum
          input =
            alphabet
              # String.charAt index
              # map String.singleton
              # Maybe.fromMaybe ""
              # (\s -> s <> string)
        in go input tracker
      else
          string
