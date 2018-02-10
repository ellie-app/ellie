module Ellie.Types.Settings
  ( Settings(..)
  , default
  ) where

import Prelude
import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype, unwrap)


newtype Settings =
  Settings
    { fontSize ∷ String
    , fontFamily ∷ String
    , theme ∷ String
    , vimMode ∷ Boolean
    }

derive instance newtypeSettings ∷ Newtype Settings _

instance encodeSettings ∷ Encode Settings where
  encode = unwrap >>> Foreign.toForeign

instance decodeSettings ∷ Decode Settings where
  decode value =
    { fontSize: _, fontFamily: _, theme: _, vimMode: _ }
      <$> (value ! "fontSize" >>= Foreign.decode)
      <*> (value ! "fontFamily" >>= Foreign.decode)
      <*> (value ! "theme" >>= Foreign.decode)
      <*> (value ! "vimMode" >>= Foreign.decode)
      <#> Settings


default ∷ Settings
default =
  Settings
    { fontSize: "14px"
    , fontFamily: "monospace"
    , theme: "Dark"
    , vimMode: false
    }