module Elm.Compiler.Error
  ( Error(..)
  , Location(..)
  , Region(..)
  , encode
  , decode
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
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
    , overview ∷ String
    , details ∷ String
    , region ∷ Region
    , level ∷ String
    }

derive instance newtypeError ∷ Newtype Error _

decode ∷ Json → Either Json.Error Error
decode value =
  { tag: _, overview: _, details: _, region: _, level: _ }
    <$> Json.decodeAtField "tag" value Json.decodeString
    <*> Json.decodeAtField "overview" value Json.decodeString
    <*> Json.decodeAtField "details" value Json.decodeString
    <*> (Json.decodeAtField "subregion" value decodeRegion <|> Json.decodeAtField "region" value decodeRegion)
    <*> Json.decodeAtField "type" value Json.decodeString
    <#> Error

encode ∷ Error → Json
encode (Error error) =
  Json.encodeObject
    [ { key: "tag", value: Json.encodeString error.tag }
    , { key: "overview", value: Json.encodeString error.overview }
    , { key: "details",  value: Json.encodeString error.details }
    , { key: "region", value: encodeRegion error.region }
    , { key: "type",  value: Json.encodeString error.level }
    ]
