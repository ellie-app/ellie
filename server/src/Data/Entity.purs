module Data.Entity
  ( Entity
  , class IdentifiedBy
  , key
  , record
  , entity
  ) where


class IdentifiedBy k r

newtype Entity k r
  = Entity (IdentifiedBy k r ⇒ { key ∷ k, record ∷ r })


key ∷ ∀ k r. IdentifiedBy k r ⇒ Entity k r → k
key (Entity info) = info.key


record ∷ ∀ k r. IdentifiedBy k r ⇒ Entity k r → r
record (Entity info) = info.record


entity ∷ ∀ k r. IdentifiedBy k r ⇒ k → r → Entity k r
entity k r =
  Entity { key: k, record: r }
