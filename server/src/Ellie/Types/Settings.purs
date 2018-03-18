module Ellie.Types.Settings
  ( Settings(..)
  , default
  , toBody
  , fromBody
  , toPostgres
  , fromPostgres
  ) where

import Prelude

import Data.Either (Either)
import Data.Json (Json)
import Data.Json as Json
import Data.Newtype (class Newtype)


newtype Settings =
  Settings
    { fontSize ∷ String
    , fontFamily ∷ String
    , theme ∷ String
    , vimMode ∷ Boolean
    }

derive instance newtypeSettings ∷ Newtype Settings _


default ∷ Settings
default =
  Settings
    { fontSize: "14px"
    , fontFamily: "monospace"
    , theme: "Dark"
    , vimMode: false
    }


toBody ∷ Settings → Json
toBody (Settings settings) =
    Json.encodeObject
      [ { key: "fontSize", value: Json.encodeString settings.fontSize }
      , { key: "fontFamily", value: Json.encodeString settings.fontFamily }
      , { key: "theme", value: Json.encodeString settings.theme }
      , { key: "vimMode", value: Json.encodeBoolean settings.vimMode }
      ]


fromBody ∷ Json → Either Json.Error Settings  
fromBody value =
    { fontSize: _, fontFamily: _, theme: _, vimMode: _ }
      <$> (Json.decodeAtField "fontSize" value Json.decodeString)
      <*> (Json.decodeAtField "fontFamily" value Json.decodeString)
      <*> (Json.decodeAtField "theme" value Json.decodeString)
      <*> (Json.decodeAtField "vimMode" value Json.decodeBoolean)
      <#> Settings


toPostgres ∷ Settings → Json
toPostgres (Settings settings) =
  Json.encodeObject
    [ { key: "font_size", value: Json.encodeString settings.fontSize }
    , { key: "font_family", value: Json.encodeString settings.fontFamily }
    , { key: "theme", value: Json.encodeString settings.theme }
    , { key: "vim_mode", value: Json.encodeBoolean settings.vimMode }
    ]


fromPostgres ∷ Json → Either Json.Error Settings
fromPostgres value =
    { fontSize: _, fontFamily: _, theme: _, vimMode: _ }
      <$> (Json.decodeAtField "font_size" value Json.decodeString)
      <*> (Json.decodeAtField "font_family" value Json.decodeString)
      <*> (Json.decodeAtField "theme" value Json.decodeString)
      <*> (Json.decodeAtField "vim_mode" value Json.decodeBoolean)
      <#> Settings
