module Data.Entity
  ( Entity
  , class IdentifiedBy
  , key
  , record
  , entity
  ) where

import Prelude
import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class (encode, decode) as Foreign
import Data.Foreign.Index ((!))

class IdentifiedBy k r

newtype Entity k r
  = Entity (IdentifiedBy k r ⇒ { key ∷ k, record ∷ r })

instance encodeEntity ∷ (Encode k, Encode r, IdentifiedBy k r) ⇒ Encode (Entity k r) where
  encode (Entity info) =
    Foreign.toForeign
      { key: Foreign.encode info.key
      , record: Foreign.encode info.record
      }

instance decodeEntity ∷ (Decode k, Decode r, IdentifiedBy k r) ⇒ Decode (Entity k r) where
  decode object = do
    info <-
      { key: _, record: _ }
        <$> (object ! "key" >>= Foreign.decode)
        <*> (object ! "record" >>= Foreign.decode)
    pure $ Entity info


key ∷ ∀ k r. IdentifiedBy k r ⇒ Entity k r -> k
key (Entity info) = info.key


record ∷ ∀ k r. IdentifiedBy k r ⇒ Entity k r -> r
record (Entity info) = info.record


entity ∷ ∀ k r. IdentifiedBy k r ⇒ k → r → Entity k r
entity key record =
  Entity { key, record }