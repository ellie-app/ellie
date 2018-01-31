module Data.BigInt
  ( BigInt
  , fromInt
  , toInt
  , shiftLeftBy
  , orWith
  ) where

import Prelude

foreign import data BigInt :: Type
foreign import _add :: BigInt -> BigInt -> BigInt
foreign import _subtract :: BigInt -> BigInt -> BigInt
foreign import _multiply :: BigInt -> BigInt -> BigInt
foreign import _divide :: BigInt -> BigInt -> BigInt
foreign import _mod :: BigInt -> BigInt -> BigInt
foreign import _compare :: BigInt -> BigInt -> Int
foreign import _fromInt :: Int -> BigInt
foreign import _eq :: BigInt -> BigInt -> Boolean
foreign import _degree :: BigInt -> Int
foreign import _toInt :: BigInt -> Int
foreign import _shiftLeftBy :: Int -> BigInt -> BigInt
foreign import _orWith :: BigInt -> BigInt -> BigInt


instance eqBigInt :: Eq BigInt where
  eq = _eq

instance ordBigInt :: Ord BigInt where
  compare l r =
    case _compare l r of
      0 -> EQ
      1 -> GT
      _ -> LT

instance semiringBigInt :: Semiring BigInt where
  add = _add
  zero = _fromInt 0
  mul = _multiply
  one = _fromInt 1

instance ringBigInt :: Ring BigInt where
  sub = _subtract

instance commutativeRingBigInt :: CommutativeRing BigInt

instance euclideanRingBigInt :: EuclideanRing BigInt where
  degree = _degree
  div = _divide
  mod = _mod


fromInt :: Int -> BigInt
fromInt = _fromInt


toInt :: BigInt -> Int
toInt = _toInt


shiftLeftBy :: Int -> BigInt -> BigInt
shiftLeftBy = _shiftLeftBy


orWith :: BigInt -> BigInt -> BigInt
orWith = _orWith