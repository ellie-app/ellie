module Data.Foreign.Class where

import Prelude

import Control.Monad.Except (except)
import Data.Either (Either(..))
import Data.Foreign (Foreign, F)
import Data.Foreign as Foreign
import Data.Foreign.Index ((!))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.StrMap (StrMap)

foreign import _null :: Foreign

null :: Foreign
null = _null


class Foreignable a where
  put :: a -> Foreign
  get :: Foreign -> F a


instance foreignableForeign :: Foreignable Foreign where
  put = id
  get = Right >>> except


instance foreignableInt :: Foreignable Int where
  put = Foreign.toForeign
  get = Foreign.readInt


instance foreignableNumber :: Foreignable Number where
  put = Foreign.toForeign
  get = Foreign.readNumber


instance foreignableString :: Foreignable String where
  put = Foreign.toForeign
  get = Foreign.readString


instance foreignableChar :: Foreignable Char where
  put = Foreign.toForeign
  get = Foreign.readChar


instance foreignableBoolean :: Foreignable Boolean where
  put = Foreign.toForeign
  get = Foreign.readBoolean


instance foreignableMaybe :: Foreignable a => Foreignable (Maybe a) where
  put maybe = Maybe.maybe null put maybe
  get value = Foreign.readNull value >>= traverse get


instance foreignableArray :: Foreignable a => Foreignable (Array a) where
  put array = Foreign.toForeign $ map put array
  get value = Foreign.readArray value >>= traverse get


instance foreignableList :: Foreignable a => Foreignable (List a) where
  put list = Foreign.toForeign $ put (List.toUnfoldable list :: Array a)
  get value = List.fromFoldable <$> (get value :: F (Array a))


instance foreignableTuple :: (Foreignable k, Foreignable v) => Foreignable (Tuple k v) where
  put (Tuple k v) =
    Foreign.toForeign $
      { "$": "Tuple"
      , _0: put k
      , _1: put v
      }

  get o = do
    d <- o ! "$" >>= Foreign.readString
    if d == "Tuple"
      then Tuple <$> (o ! "_0" >>= get) <*> (o ! "_1" >>= get)
      else Foreign.fail (Foreign.TypeMismatch "Tuple" d)


instance foreignableStrMap :: Foreignable v => Foreignable (StrMap v) where
  put = Foreign.toForeign
  get = Foreign.unsafeReadTagged "Object"


instance foreignableMap :: (Ord k, Foreignable k, Foreignable v) => Foreignable (Map k v) where
  put dict =
    Foreign.toForeign $
      { "$": "Map"
      , _0: put (Map.toUnfoldable dict :: Array (Tuple k v))
      }

  get o = do
    d <- o ! "$" >>= Foreign.readString
    if d == "Map"
      then o ! "_0" >>= (\v -> map Map.fromFoldable $ (get v :: F (Array (Tuple k v))))
      else Foreign.fail (Foreign.TypeMismatch "Map" d)


instance foreignableSet :: (Ord a, Foreignable a) => Foreignable (Set a) where
  put set =
    Foreign.toForeign $
      { "$": "Set"
      , _0: put (Set.toUnfoldable set :: Array a)
      }

  get o = do
    d <- o ! "$" >>= Foreign.readString
    if d == "Set"
      then o ! "_0" >>= (\v -> map Set.fromFoldable $ (get v :: F (Array a)))
      else Foreign.fail (Foreign.TypeMismatch "Set" d)
