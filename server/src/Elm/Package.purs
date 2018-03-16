module Elm.Package
  ( Package(..)
  -- eqPackage
  -- ordPackage
  -- newtypePackage
  -- showPackage
  -- toStringPackage
  -- fromJsonPackage
  -- toJsonPackage
  -- toFieldPackage
  ) where

import Prelude

import Data.Json as Json
import Data.Newtype (class Newtype)
import Data.String.Class (class ToString)
import Data.String.Class (toString) as String
import Elm.Name (Name(..))
import Elm.Version (Version)
import Server.Action (class IsBody)
import Server.Action as Action
import System.Postgres (class ToValue, class FromResult)
import System.Postgres as Postgres


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

instance fromResultPackage ∷ FromResult Package where
  fromResult value =
    { name: _, version: _ }
      <$> Json.decodeAtField "name" value Postgres.fromResult
      <*> Json.decodeAtField "version" value Postgres.fromResult
      <#> Package

instance toValuePackage ∷ ToValue Package where
  toValue (Package p) =
    Json.encodeObject
      [ { key: "name", value: Postgres.toValue p.name }
      , { key: "version", value: Postgres.toValue p.version }
      ]

instance isBodyPackage ∷ IsBody Package where
  toBody (Package p) =
    Json.encodeArray id
      [ Action.toBody p.name
      , Action.toBody p.version
      ]
  fromBody value =
    { name: _, version: _ }
      <$> Json.decodeAtIndex 0 value Action.fromBody
      <*> Json.decodeAtIndex 1 value Action.fromBody
      <#> Package
