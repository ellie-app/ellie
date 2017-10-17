module Data.Hash.Hashable
  ( class Hashable
  , Hash
  , compute
  , hash
  )
  where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2)

foreign import data Hash :: Type
foreign import _hashString :: ∀ a. a -> Int
foreign import _hashArray :: ∀ a. Fn2 (a -> Int) (Array a) Int
foreign import _hashMemoized :: ∀ a. Fn2 (a -> Int) a Hash
foreign import _showHash :: Hash -> String

class Hashable a where
  compute :: a -> Int

instance hashableString :: Hashable String where
  compute = _hashString

instance hashableInt :: Hashable Int where
  compute = id

instance hashableNumber :: Hashable Number where
  compute = show >>> compute

instance hashableBoolean :: Hashable Boolean where
  compute false = 0
  compute true = 1

instance hashableArray :: Hashable a => Hashable (Array a) where
  compute = runFn2 _hashArray compute

hash :: ∀ a. Hashable a => a -> Hash
hash a =
  runFn2 _hashMemoized compute a
