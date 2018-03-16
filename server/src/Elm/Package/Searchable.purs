module Elm.Package.Searchable
  ( Searchable(..)
  -- toJsonSearchable
  -- indexableSearchable
  ) where

import Prelude
import Data.Indexable (class Indexable)
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.String.Class as String
import Elm.Package (Package(..))
import Elm.Name (Name(..))
import Elm.Version as Version
import Data.Json as Json
import Server.Action (class IsBody)
import Server.Action as Action


newtype Searchable =
  Searchable
    { package ∷ Package
    , description ∷ String
    }

instance isBodySearchable ∷ IsBody Searchable where
  toBody (Searchable s) =
    Json.encodeObject
      [ { key: "package", value: Action.toBody s.package }
      , { key: "description", value: Json.encodeString s.description }
      ]
  fromBody value =
    { package: _, description: _ }
      <$> Json.decodeAtField "package" value Action.fromBody
      <*> Json.decodeAtField "description" value Action.fromBody
      <#> Searchable

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
