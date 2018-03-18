module Elm.Package.Searchable
  ( Searchable(..)
  , latestPackage
  , toBody
  , fromBody
  , toPostgres
  , fromPostgres
  ) where

import Prelude

import Data.Either (Either)
import Data.Foldable (maximum)
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe as Maybe
import Elm.Name (Name)
import Elm.Name as Name
import Elm.Package (Package(..))
import Elm.Version (Version)
import Elm.Version as Version


newtype Searchable =
  Searchable
    { name ∷ Name
    , summary ∷ String
    , versions ∷ Array Version
    }


latestPackage ∷ Searchable → Package
latestPackage (Searchable s) =
  Package
    { name: s.name
    , version: Maybe.fromMaybe Version.zero $ maximum s.versions
    }



-- SERIALIZATIONS


toBody ∷ Searchable → Json
toBody (Searchable s) =
  Json.encodeObject
    [ { key: "name", value: Name.toBody s.name }
    , { key: "summary", value: Json.encodeString s.summary }
    , { key: "versions", value: Json.encodeArray Version.toBody s.versions }
    ]


fromBody ∷ Json → Either Json.Error Searchable
fromBody value =
  { name: _, summary: _, versions: _ }
    <$> Json.decodeAtField "name" value Name.fromBody
    <*> Json.decodeAtField "summary" value Json.decodeString
    <*> Json.decodeAtField "versions" value (Json.decodeArray Version.fromBody)
    <#> Searchable


toPostgres ∷ Searchable → Json
toPostgres (Searchable s) =
  Json.encodeObject
    [ { key: "name", value: Name.toPostgres s.name }
    , { key: "summary", value: Json.encodeString s.summary }
    , { key: "versions", value: Json.encodeArray Version.toPostgres s.versions }
    ]


fromPostgres ∷ Json → Either Json.Error Searchable
fromPostgres value =
  { name: _, summary: _, versions: _ }
    <$> Json.decodeAtField "name" value Name.fromPostgres
    <*> Json.decodeAtField "summary" value Json.decodeString
    <*> Json.decodeAtField "versions" value (Json.decodeArray Version.fromPostgres)
    <#> Searchable
