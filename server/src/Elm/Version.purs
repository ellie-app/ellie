module Elm.Version
  ( Version(..)
  , zero
  , toString
  , fromString
  , toBody
  , fromBody
  , toPostgres
  , fromPostgres
  )where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (split) as String
import Data.String.Class (toString) as String
import Data.Traversable (traverse)


newtype Version =
  Version
    { major ∷ Int
    , minor ∷ Int
    , patch ∷ Int
    }

derive instance eqVersion ∷ Eq Version
derive instance ordVersion ∷ Ord Version


zero ∷ Version
zero =
  Version { major: 0, minor: 0, patch: 0 }


-- SERIALIZATIONS


toString ∷ Version → String
toString (Version { major, minor, patch }) =
  show major <> "." <> show minor <> "." <> show patch


fromString ∷ String → Maybe Version
fromString string =
  let maybeInts = traverse Int.fromString $ String.split (Pattern ".") string
  in case maybeInts of
    Just [major, minor, patch] → Just $ Version { major, minor, patch }
    _ →  Nothing


fromBody ∷ Json → Either Json.Error Version
fromBody value = do
  string ← Json.decodeString value
  let maybeInts = traverse Int.fromString $ String.split (Pattern ".") string
  case maybeInts of
    Just [major, minor, patch] → Right $ Version { major, minor, patch }
    _ → Left $ Json.Failure "Expecting version string in format MAJOR.MINOR.PATCH." value


toBody ∷ Version → Json
toBody (Version { major, minor, patch }) =
    Json.encodeString $ String.toString major <> "." <> String.toString minor <> "." <> String.toString patch


fromPostgres ∷ Json → Either Json.Error Version
fromPostgres value =
    { major: _, minor: _, patch: _ }
      <$> Json.decodeAtField "major" value Json.decodeInt
      <*> Json.decodeAtField "minor" value Json.decodeInt
      <*> Json.decodeAtField "patch" value Json.decodeInt
      <#> Version

toPostgres ∷ Version → Json
toPostgres (Version v) =
  Json.encodeObject
    [ { key: "major", value: Json.encodeInt v.major }
    , { key: "minor", value: Json.encodeInt v.minor }
    , { key: "patch", value: Json.encodeInt v.patch }
    ]
