module Ellie.Types.Settings
  ( Settings(..)
  , default
  ) where

import Prelude
import Data.Json as Json
import Data.Newtype (class Newtype)
import Server.Action (class IsBody)
import Server.Action as Action
import System.Postgres (class ToValue, class FromResult)
import System.Postgres as Postgres


newtype Settings =
  Settings
    { fontSize ∷ String
    , fontFamily ∷ String
    , theme ∷ String
    , vimMode ∷ Boolean
    }

derive instance newtypeSettings ∷ Newtype Settings _

instance isBodySettings ∷ IsBody Settings where
  toBody (Settings settings) =
    Json.encodeObject
      [ { key: "fontSize", value: Action.toBody settings.fontSize }
      , { key: "fontFamily", value: Action.toBody settings.fontFamily }
      , { key: "theme", value: Action.toBody settings.theme }
      , { key: "vimMode", value: Action.toBody settings.vimMode }
      ]
  fromBody value =
    { fontSize: _, fontFamily: _, theme: _, vimMode: _ }
      <$> (Json.decodeAtField "fontSize" value Postgres.fromResult)
      <*> (Json.decodeAtField "fontFamily" value Postgres.fromResult)
      <*> (Json.decodeAtField "theme" value Postgres.fromResult)
      <*> (Json.decodeAtField "vimMode" value Postgres.fromResult)
      <#> Settings

instance toValueSettings ∷ ToValue Settings where
  toValue (Settings settings) =
    Json.encodeObject
      [ { key: "font_size", value: Postgres.toValue settings.fontSize }
      , { key: "font_family", value: Postgres.toValue settings.fontFamily }
      , { key: "theme", value: Postgres.toValue settings.theme }
      , { key: "vim_mode", value: Postgres.toValue settings.vimMode }
      ]

instance fromResultSettings ∷ FromResult Settings where
  fromResult value =
    { fontSize: _, fontFamily: _, theme: _, vimMode: _ }
      <$> (Json.decodeAtField "font_size" value Postgres.fromResult)
      <*> (Json.decodeAtField "font_family" value Postgres.fromResult)
      <*> (Json.decodeAtField "theme" value Postgres.fromResult)
      <*> (Json.decodeAtField "vim_mode" value Postgres.fromResult)
      <#> Settings


default ∷ Settings
default =
  Settings
    { fontSize: "14px"
    , fontFamily: "monospace"
    , theme: "Dark"
    , vimMode: false
    }
