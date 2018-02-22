module Data.UniqueId (UniqueId(..)) where

import Prelude
import Data.String.Class (class ToString)
import Data.Foreign.Class (class Encode)

newtype UniqueId
  = UniqueId String

derive newtype instance toStringUniqueId ∷ ToString UniqueId
derive newtype instance encodeUniqueId ∷ Encode UniqueId
derive newtype instance eqUniqueId ∷ Eq UniqueId
derive newtype instance ordUniqueId ∷ Ord UniqueId
derive newtype instance showUniqueId ∷ Show UniqueId
