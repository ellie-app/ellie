module Ellie.Elm.Package.Version where

import Prelude

import Data.Foreign (ForeignError(ForeignError), fail) as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
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
