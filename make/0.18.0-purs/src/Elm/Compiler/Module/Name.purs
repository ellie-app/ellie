module Elm.Compiler.Module.Name where

import Prelude
import Elm.Package as Package
import Data.String as String


newtype Raw = Raw (Array String)


instance showRaw :: Show Raw where
  show (Raw segments) =
    String.joinWith "." segments


isNative :: Raw -> Bool
isNative (Raw ("Native" : _ )) = True
isNative _ = False


newtype Canonical = Canonical
  { _package :: Package.Name
  , _module :: Raw
  }


instance showCanonical :: Show Canonical where
  show (Canonical { package, _module }) =
    show _module


inVirtualDom :: Raw -> Canonical
inVirtualDom raw =
  Canonical Package.virtualDom raw


inCore :: Raw -> Canonical
inCore raw =
  Canonical Package.core raw


inHtml :: Raw -> Canonical
inHtml raw =
  Canonical Package.html raw
