module Elm.Package
  ( Name(..)
  , core
  , virtualDom
  , html
  , toFilePath
  , Version(..)
  , Package(..)
  )
  where

import System.FilePath (FilePath, (</>))
import Data.Either
import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, encodeJson, toString, fromString, toArray, fromArray)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Data.String (split, Pattern(..))
import Data.String.Read (class Read, class Zero, read)
import Prelude (class Show, class Ord, class Eq, bind, show, (#), ($), (>>=))
import Data.Generic (class Generic)

-- NAME


newtype Name = Name
  { user :: String
  , project :: String
  }

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name
derive instance genericName :: Generic Name


instance readName :: Read Name where
  read input =
    case split (Pattern "/") input of
      [ user, project ] ->
        Just $ Name { user, project }

      _ ->
        Nothing


instance zeroName :: Zero Name where
  zero =
    Name { user: "user", project: "project" }


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


toFilePath :: Name -> FilePath
toFilePath (Name { user, project }) =
     user </> project


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


newtype Version =
  Version
    { major :: Int
    , minor :: Int
    , patch :: Int
    }

derive instance eqVersion :: Eq Version
derive instance ordVersion :: Ord Version
derive instance genericVersion :: Generic Version


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


newtype Package =
  Package
    { name :: Name
    , version :: Version
    }

derive instance eqPackage :: Eq Package
derive instance ordPackage :: Ord Package
derive instance genericPackage :: Generic Package


instance showPackage :: Show Package where
  show (Package { name, version }) =
    (show name) <> "@" <> (show version)


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
