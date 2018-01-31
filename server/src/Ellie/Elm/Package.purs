module Ellie.Elm.Package where

import Prelude
import Data.Indexable (class Indexable)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class as Foreign
import Data.Newtype (class Newtype)
import Ellie.Elm.Package.Name (Name(..))
import Ellie.Elm.Package.Version (Version(..))

newtype Package =
  Package
    { name :: Name
    , version :: Version
    }

derive instance eqPackage :: Eq Package
derive instance ordPackage :: Ord Package
derive instance newtypePackage :: Newtype Package _

instance decodePackage :: Decode Package where
  decode json = do
    array <- Foreign.decode json
    case array of
      [name, version] ->
        { name: _, version: _ }
          <$> Foreign.decode name
          <*> Foreign.decode version
          <#> Package
      _ ->
        Foreign.fail $ Foreign.ForeignError "Expected an array with 2 elements"

instance encodePackage :: Encode Package where
  encode (Package p) =
    Foreign.encode
      [ Foreign.encode p.name
      , Foreign.encode p.version
      ]

instance indexablePackage :: Indexable { name :: String, version :: String } Package where
  toDocument (Package p) =
    { name: "hi", version: "hello" }

  fromDocument { name, version } =
    Package { name: Name { user: "lol", project: "lol" }, version: Version { major: 0, minor: 0, patch: 0 } }