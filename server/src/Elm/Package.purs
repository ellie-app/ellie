module Elm.Package where

import Prelude

import Data.Foreign as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class as Foreign
import Data.Indexable (class Indexable)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap)
import Data.String.Class (class ToString)
import Data.String.Class (toString) as String
import Elm.Package.Name (Name(..))
import Elm.Package.Version (Version(..))
import Elm.Package.Version as Version


newtype Package =
  Package
    { name ∷ Name
    , version ∷ Version
    }

derive instance eqPackage ∷ Eq Package
derive instance ordPackage ∷ Ord Package
derive instance newtypePackage ∷ Newtype Package _

instance showPackage ∷ Show Package where
  show (Package { name: Name name, version }) =
    "Package { name: Name { user: " <> show name.user <> ", project: " <> show name.project <> " }, version: Version " <> show version <> " }"

instance toStringPackage ∷ ToString Package where
  toString (Package { name, version }) =
    String.toString name <> "@" <> String.toString version

instance decodePackage ∷ Decode Package where
  decode json = do
    array ← Foreign.decode json
    case array of
      [name, version] →
        { name: _, version: _ }
          <$> Foreign.decode name
          <*> Foreign.decode version
          <#> Package
      _ →
        Foreign.fail $ Foreign.ForeignError "Expected an array with 2 elements"

instance encodePackage ∷ Encode Package where
  encode (Package p) =
    Foreign.encode
      [ Foreign.encode p.name
      , Foreign.encode p.version
      ]
