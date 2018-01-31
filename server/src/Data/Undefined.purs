module Data.Undefined (Undefined, undefined) where

import Data.Foreign (unsafeFromForeign) as Foreign
import Data.Foreign.NullOrUndefined (undefined) as Foreign

foreign import data Undefined ∷ Type

undefined ∷ Undefined
undefined =
  Foreign.unsafeFromForeign Foreign.undefined