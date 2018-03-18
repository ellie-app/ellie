module Elm.Package.Searchable
  ( Searchable(..)
  , toBody
  , toPostgres
  , fromPostgres
  ) where

import Prelude

import Data.Either (Either)
import Data.Json (Json)
import Data.Json as Json
import Elm.Package (Package)
import Elm.Package as Package


newtype Searchable =
  Searchable
    { package ∷ Package
    , description ∷ String
    }


-- SERIALIZATIONS


toBody ∷ Searchable → Json
toBody (Searchable s) =
    Json.encodeObject
      [ { key: "package", value: Package.toBody s.package }
      , { key: "description", value: Json.encodeString s.description }
      ]


toPostgres ∷ Searchable → Json
toPostgres (Searchable s) =
  Json.encodeObject
    [ { key: "package", value: Package.toPostgres s.package }
    , { key: "description", value: Json.encodeString s.description }
    ]


fromPostgres ∷ Json → Either Json.Error Searchable
fromPostgres value =
  { package: _,  description: _ }
    <$> Json.decodeAtField "package" value Package.fromPostgres
    <*> Json.decodeAtField "description" value Json.decodeString
    <#> Searchable
