module Data.UniqueId (UniqueId(..)) where

import Data.String.Class (class ToString)
import Data.Foreign.Class (class Encode)

newtype UniqueId
  = UniqueId String

derive newtype instance toStringUniqueId ∷ ToString UniqueId
derive newtype instance encodeUniqueId ∷ Encode UniqueId
