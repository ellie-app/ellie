module Elm.Version
  ( Version(..)
  , zero
  -- eqVersion
  -- ordVersion
  -- showVersion
  -- toStringVersion
  -- fromJsonVersion
  -- toJsonVersion
  -- toFieldVersion
  )where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (split) as String
import Data.String.Class (class ToString, class FromString)
import Data.String.Class (toString, fromString) as String
import Data.Traversable (traverse)
import Server.Action (class IsBody)
import Server.Action as Action
import System.Postgres (class FromResult, class ToValue)
import System.Postgres as Postgres


newtype Version =
  Version
    { major ∷ Int
    , minor ∷ Int
    , patch ∷ Int
    }

derive instance eqVersion ∷ Eq Version
derive instance ordVersion ∷ Ord Version

instance showVersion ∷ Show Version where
  show (Version { major, minor, patch }) =
    show major <> "." <> show minor <> "." <> show patch

instance toStringVersion ∷ ToString Version where
  toString (Version { major, minor, patch }) =
    String.toString major <> "." <> String.toString minor <> "." <> String.toString patch

instance fromStringVersion ∷ FromString Version where
  fromString string =
    let maybeInts = traverse String.fromString $ String.split (Pattern ".") string
    in case maybeInts of
      Just [major, minor, patch] →
        Just $ Version { major, minor, patch }
      _ →
        Nothing

instance isBodyVersion ∷ IsBody Version where
  fromBody value = do
    string ← Json.decodeString value
    let maybeInts = traverse Int.fromString $ String.split (Pattern ".") string
    case maybeInts of
      Just [major, minor, patch] → Right $ Version { major, minor, patch }
      _ → Left $ Json.Failure "Expecting version string in format MAJOR.MINOR.PATCH." value
  toBody (Version { major, minor, patch }) =
    Json.encodeString $ String.toString major <> "." <> String.toString minor <> "." <> String.toString patch

instance fromResultVersion ∷ FromResult Version where
  fromResult value =
    { major: _, minor: _, patch: _ }
      <$> Json.decodeAtField "major" value Postgres.fromResult
      <*> Json.decodeAtField "minor" value Postgres.fromResult
      <*> Json.decodeAtField "patch" value Postgres.fromResult
      <#> Version

instance toValueVersion ∷ ToValue Version where
  toValue (Version v) =
    Json.encodeObject
      [ { key: "major", value: Postgres.toValue v.major }
      , { key: "minor", value: Postgres.toValue v.minor }
      , { key: "patch", value: Postgres.toValue v.patch }
      ]


zero ∷ Version
zero =
  Version { major: 0, minor: 0, patch: 0 }
