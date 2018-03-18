module Elm.Package
  ( Package(..)
  , toPostgres
  , fromPostgres
  , toBody
  , fromBody
  ) where

import Prelude

import Data.Either (Either)
import Data.Json (Json)
import Data.Json as Json
import Data.Newtype (class Newtype)
import Elm.Name (Name)
import Elm.Name as Name
import Elm.Version (Version)
import Elm.Version as Version


newtype Package =
  Package
    { name ∷ Name
    , version ∷ Version
    }

derive instance eqPackage ∷ Eq Package
derive instance ordPackage ∷ Ord Package
derive instance newtypePackage ∷ Newtype Package _


-- SERIALIZATIONS


fromPostgres ∷ Json → Either Json.Error Package
fromPostgres value =
    { name: _, version: _ }
      <$> Json.decodeAtField "name" value Name.fromPostgres
      <*> Json.decodeAtField "version" value Version.fromPostgres
      <#> Package


toPostgres ∷ Package → Json
toPostgres (Package p) =
    Json.encodeObject
      [ { key: "name", value: Name.toPostgres p.name }
      , { key: "version", value: Version.toPostgres p.version }
      ]


toBody ∷ Package → Json
toBody (Package p) =
  Json.encodeArray id
    [ Name.toBody p.name
    , Version.toBody p.version
    ]


fromBody ∷ Json → Either Json.Error Package
fromBody value =
  { name: _, version: _ }
    <$> Json.decodeAtIndex 0 value Name.fromBody
    <*> Json.decodeAtIndex 1 value Version.fromBody
    <#> Package
