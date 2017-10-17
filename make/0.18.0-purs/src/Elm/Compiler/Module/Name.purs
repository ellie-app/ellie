module Elm.Compiler.Module.Name where

import Prelude (class Eq, class Show, class Ord, show)
import Elm.Package (Name, virtualDom, core, html)
import Data.String as String
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)

newtype Raw =
  Raw (Array String)

derive instance eqRaw :: Eq Raw
derive instance ordRaw :: Ord Raw
derive instance genericRaw :: Generic Raw
instance decodeJsonRaw :: DecodeJson Raw where decodeJson = gDecodeJson
instance encodeJsonRaw :: EncodeJson Raw where encodeJson = gEncodeJson

instance showRaw :: Show Raw where
  show (Raw segments) =
    String.joinWith "." segments


isNative :: Raw -> Boolean
isNative (Raw ["Native", _]) = true
isNative _ = false


newtype Canonical = Canonical
  { package :: Name
  , modul :: Raw
  }

derive instance eqCanonical :: Eq Canonical
derive instance ordCanonical :: Ord Canonical
derive instance genericCanonical :: Generic Canonical
instance decodeJsonCanonical :: DecodeJson Canonical where decodeJson = gDecodeJson
instance encodeJsonCanonical :: EncodeJson Canonical where encodeJson = gEncodeJson

instance showCanonical :: Show Canonical where
  show (Canonical { package, modul }) =
    show modul


inVirtualDom :: Raw -> Canonical
inVirtualDom raw =
  Canonical { package: virtualDom, modul: raw }


inCore :: Raw -> Canonical
inCore raw =
  Canonical { package: core, modul: raw }


inHtml :: Raw -> Canonical
inHtml raw =
  Canonical { package: html, modul: raw }
