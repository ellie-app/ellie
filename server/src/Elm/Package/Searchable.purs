module Elm.Package.Searchable where

import Prelude

import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class (encode) as Foreign
import Data.Indexable (class Indexable)
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.String.Class as String
import Elm.Package (Package(..))
import Elm.Package.Name (Name(..))
import Elm.Package.Version (Version)
import Elm.Package.Version as Version


newtype Searchable =
  Searchable
    { package ∷ Package
    , description ∷ String
    }

instance encodeSearchable ∷ Encode Searchable where
  encode (Searchable s) =
    Foreign.toForeign
      { package: Foreign.encode s.package
      , description: s.description
      }

instance indexableSearchable ∷ Indexable { id ∷ String, user ∷ String, project ∷ String, version ∷ String, description ∷ String } Searchable where
  toDocument (Searchable s) =
    { user: s.package # unwrap # _.name # unwrap # _.user
    , project: s.package # unwrap # _.name # unwrap # _.project
    , version: s.package # unwrap # _.version # String.toString
    , description: s.description
    , id: String.toString s.package
    }

  fromDocument { user, project, version, description } =
    Searchable
      { package:
          Package
            { name: Name { user, project }
            , version: Maybe.fromMaybe Version.zero $ String.fromString version
            }
      , description
      }
