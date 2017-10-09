module Elm.Package where

import Prelude
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, encodeJson, toString, fromString, toArray, fromArray)
import Data.String.Read (class Read, class Zero, read)
import Data.String (split, Pattern(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Int as Int
import Data.Monoid
import Control.Monad (bind)
import Data.Either

-- NAME

newtype Name = Name
  { user :: String
  , project :: String
  }


instance readName :: Read Name where
  read input =
    case split (Pattern "/") input of
      [ user, project ] ->
        Just $ Name { user, project }

      _ ->
        Nothing

instance showName :: Show Name where
  show (Name { user, project }) =
    user <> "/" <> project


instance decodeJsonName :: DecodeJson Name where
  decodeJson input =
    input
      # toString
      >>= read
      # maybe (Left "Package names must be in the form user/project") Right


instance encodeJsonName :: EncodeJson Name where
  encodeJson name =
    name
      # show
      # fromString


core :: Name
core =
  Name { user: "elm-lang", project: "core" }


virtualDom :: Name
virtualDom =
  Name { user: "elm-lang", project: "virtual-dom" }


html :: Name
html =
  Name { user: "elm-lang", project: "html" }


-- VERSION

newtype Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }


instance readVersion :: Read Version where
  read input =
    case split (Pattern ".") input of
      [majorString, minorString, patchString] -> do
        major <- Int.fromString majorString
        minor <- Int.fromString minorString
        patch <- Int.fromString patchString
        Just $ Version { major, minor, patch }

      _ ->
        Nothing


instance showVersion :: Show Version where
  show (Version { major, minor, patch }) =
    show major
      <> "."
      <> show minor
      <> "."
      <> show patch


instance zeroVersion :: Zero Version where
  zero =
    Version { major: 0, minor: 0, patch: 0 }


instance decodeJsonVersion :: DecodeJson Version where
  decodeJson value =
    value
      # toString
      >>= read
      # maybe (Left "Versions must be in the form MAJOR.MINOR.PATCH") Right


instance encodeJsonVersion :: EncodeJson Version where
  encodeJson version =
    version
      # show
      # fromString

-- PACKAGE

newtype Package = Package
  { name :: Name
  , version :: Version
  }


instance showPackage :: Show Package where
  show (Package { name, version }) =
    show name <> "@" <> show version


instance decodeJsonPackage :: DecodeJson Package where
  decodeJson value =
    case toArray value of
      Just [nameJson, versionJson] -> do
        name <- decodeJson nameJson
        version <- decodeJson versionJson
        Right $ Package { name, version }

      _ ->
        Left "Package must be an array of [name, version]"

instance encodeJsonpackage :: EncodeJson Package where
  encodeJson (Package { name, version }) =
    fromArray [ encodeJson name, encodeJson version ]
