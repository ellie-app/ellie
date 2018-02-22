module Data.String.Murmur (hash) where

foreign import _hash ∷ String → String


hash ∷ String → String
hash = _hash
