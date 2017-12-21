module Data.Blob
  ( BLOB
  , Blob
  , createObjectUrl
  , revokeObjectUrl
  , create
  ) where


import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, kind Effect)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Url (Url)
import Data.Url as Url


foreign import data BLOB :: Effect
foreign import data Blob :: Type
foreign import _createObjectUrl :: ∀ e. Blob -> Eff (blob :: BLOB | e) String
foreign import _revokeObjectUrl :: ∀ e. Fn2 Unit String (Eff (blob :: BLOB | e) Unit)
foreign import _create :: ∀ e. Fn2 (Array String) String (Eff (blob :: BLOB | e) Blob)


create :: ∀ e. Array String -> String -> Eff (blob :: BLOB | e) Blob
create =
  runFn2 _create


createObjectUrl :: ∀ e. Blob -> Eff (blob :: BLOB | e) Url
createObjectUrl blob =
  blob
    # _createObjectUrl
    <#> (\url -> Url.crossOrigin url [] [])


revokeObjectUrl :: ∀ e. Url -> Eff (blob :: BLOB | e) Unit
revokeObjectUrl url =
  url
    # show
    # runFn2 _revokeObjectUrl unit


instance foreignableBlob :: Foreignable Blob where
  put = Foreign.toForeign
  get input = Foreign.unsafeReadTagged "Blob" input <|> Foreign.unsafeReadTagged "File" input