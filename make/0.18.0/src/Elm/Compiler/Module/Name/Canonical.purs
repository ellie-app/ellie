module Elm.Compiler.Module.Name.Canonical where

import Ellie.Prelude
import Elm.Compiler.Module.Name.Raw (Raw)
import Elm.Package.Name (Name)
import Elm.Package.Name as Name
import Data.Foreign as Foreign
import Data.Foreign.Index ((!))
import Data.Foreign.Class (class Foreignable)
import Data.Foreign.Class as Foreign
import Data.Newtype (class Newtype)

newtype Canonical =
  Canonical
    { package :: Name
    , modul :: Raw
    }

derive instance newtypeCanonical :: Newtype Canonical _
derive instance eqCanonical :: Eq Canonical
derive instance ordCanonical :: Ord Canonical

instance foreignableCanonical :: Foreignable Canonical where
  put (Canonical { package, modul }) =
    Foreign.toForeign { package: Foreign.put package, modul: Foreign.put modul }

  get value =
    { package: _, modul: _ }
      <$> (value ! "package" >>= Foreign.get)
      <*> (value ! "module" >>= Foreign.get)
      <#> Canonical


instance showCanonical :: Show Canonical where
  show (Canonical { package, modul }) =
    show modul

inVirtualDom :: Raw -> Canonical
inVirtualDom raw =
  Canonical { package: Name.virtualDom, modul: raw }

inCore :: Raw -> Canonical
inCore raw =
  Canonical { package: Name.core, modul: raw }

inHtml :: Raw -> Canonical
inHtml raw =
  Canonical { package: Name.html, modul: raw }
