module Ellie.Elm.Package.Version where

import Prelude

import Data.Foreign (ForeignError(ForeignError), fail) as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Class (class ToString, class FromString)
import Data.String.Class (toString, fromString) as String
import Data.Traversable (traverse)

newtype Version =
  Version
    { major :: Int
    , minor :: Int
    , patch :: Int
    }

derive instance eqVersion :: Eq Version
derive instance ordVersion :: Ord Version

instance showVersion :: Show Version where
  show (Version { major, minor, patch }) =
    show major <> "." <> show minor <> "." <> show patch

instance toStringVersion ∷ ToString Version where
  toString (Version { major, minor, patch }) =
    String.toString major <> "." <> String.toString minor <> "." <> String.toString patch

instance fromStringVersion ∷ FromString Version where
  fromString string =
    let maybeInts = traverse String.fromString $ String.split (Pattern ".") string
    in case maybeInts of
      Just [major, minor, patch] ->
        Just $ Version { major, minor, patch }
      _ ->
        Nothing

instance decodeVersion :: Decode Version where
  decode input = do
    string <- Foreign.decode input
    let maybeInts = traverse Int.fromString $ String.split (Pattern ".") string
    case maybeInts of
      Just [major, minor, patch] ->
        pure $ Version { major, minor, patch }
      _ ->
        Foreign.fail $ Foreign.ForeignError $ "Expected version in format MAJOR.MINOR.PATCH. Got " <> string

instance encodeVersion :: Encode Version where
  encode (Version { major, minor, patch }) =
    Foreign.encode $ show major <> "." <> show minor <> "." <> show patch


zero ∷ Version
zero =
  Version { major: 0, minor: 0, patch: 0 }