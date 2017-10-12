module Elm.Compiler.Module.Name where

import Prelude
import Elm.Package as Package
import Data.String as String

newtype Raw = Raw (Array String)


instance showRaw :: Show Raw where
  show (Raw segments) =
    String.joinWith "." segments


isNative :: Raw -> Boolean
isNative (Raw ["Native", _]) = true
isNative _ = false


newtype Canonical = Canonical
  { _package :: Package.Name
  , _module :: Raw
  }


instance showCanonical :: Show Canonical where
  show (Canonical { _package, _module }) =
    show _module


inVirtualDom :: Raw -> Canonical
inVirtualDom raw =
  Canonical { _package: Package.virtualDom, _module: raw }


inCore :: Raw -> Canonical
inCore raw =
  Canonical { _package: Package.core, _module: raw }


inHtml :: Raw -> Canonical
inHtml raw =
  Canonical { _package: Package.html, _module: raw }
