module Elm.Compiler.Error
  ( Error(..)
  , Location(..)
  , Region(..)
  , encode
  , decode
  ) where

import Prelude

import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Json (Json)
import Data.Json as Json
import Control.Alt ((<|>))


newtype Location =
  Location
    { line ∷ Int
    , column ∷ Int
    }

decodeLocation ∷ Json → Either Json.Error Location
decodeLocation value =
  { line: _, column: _ }
    <$> Json.decodeAtField "line" value Json.decodeInt
    <*> Json.decodeAtField "column" value Json.decodeInt
    <#> Location

encodeLocation ∷ Location → Json
encodeLocation (Location location) =
  Json.encodeObject
    [ { key: "line", value: Json.encodeInt location.line }
    , { key: "column", value: Json.encodeInt location.column }
    ]


newtype Region =
  Region
    { start ∷ Location
    , end ∷ Location
    }

decodeRegion ∷ Json → Either Json.Error Region
decodeRegion value =
  { start: _, end: _ }
    <$> Json.decodeAtField "start" value decodeLocation
    <*> Json.decodeAtField "end" value decodeLocation
    <#> Region

encodeRegion ∷ Region → Json
encodeRegion (Region { start, end }) =
    Json.encodeObject
      [ { key: "start", value: encodeLocation start }
      , { key: "end", value: encodeLocation end }
      ]


newtype Error =
  Error
    { tag ∷ String
    , message ∷ String
    , region ∷ Region
    , level ∷ String
    }

derive instance newtypeError ∷ Newtype Error _

decode ∷ Json → Either Json.Error Error
decode value =
  { tag: _, message: _, region: _, level: _ }
    <$> Json.decodeAtField "tag" value Json.decodeString
    <*> Json.decodeAtField "message" value Json.decodeString
    <*> (Json.decodeAtField "subregion" value decodeRegion <|> Json.decodeAtField "region" value decodeRegion)
    <*> Json.decodeAtField "type" value Json.decodeString
    <#> Error

encode ∷ Error → Json
encode (Error error) =
  Json.encodeObject
    [ { key: "tag", value: Json.encodeString error.tag }
    , { key: "message",  value: Json.encodeString error.message }
    , { key: "region", value: encodeRegion error.region }
    , { key: "type",  value: Json.encodeString error.level }
    ]
