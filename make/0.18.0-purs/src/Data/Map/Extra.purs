module Data.Map.Extra
  ( mapEither
  , mapEitherWithKey
  , (!)
  , unsafeLookupFlipped
  , unsafeLookup
  , traverseWithKey
  , toArray
  , unionsWith
  , mapKeys
  , insertWith
  )
  where

import Ellie.Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Partial

infixl 9 unsafeLookupFlipped as !


insertWith :: ∀ k v. Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v m =
  Map.alter (map (f v) >>> Maybe.fromMaybe v >>> Just) k m


unsafeLookupFlipped :: ∀ k v. Ord k => Map k v -> k -> v
unsafeLookupFlipped =
  flip unsafeLookup


unsafeLookup :: ∀ k v. Ord k => k -> Map k v -> v
unsafeLookup k v =
  case Map.lookup k v of
    Just a -> a
    Nothing -> Partial.unsafeCrashWith "Tried to unsafely look up a key in a Map"


traverseWithKey :: forall t k a b. Ord k => Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey f map =
  map
    # (Map.toUnfoldable :: Map k a -> Array (Tuple k a))
    # traverse (\(Tuple k v) -> Tuple k <$> f k v)
    <#> Map.fromFoldable


mapEither ::
  ∀  k a b c
  .  Ord k
  => (a -> Either b c)
  -> Map k a
  -> { left :: Map k b, right :: Map k c }
mapEither f map =
  mapEitherWithKey (\_ v -> f v) map


mapEitherWithKey ::
  ∀  k a b c
  .  Ord k
  => (k -> a -> Either b c)
  -> Map k a
  -> { left :: Map k b, right :: Map k c }
mapEitherWithKey f map =
  map
    # toArray
    # Array.foldr fold { left: Map.empty, right: Map.empty }
  where
    fold (Tuple k v) { left, right } =
      case f k v of
        Left out ->
          { left: Map.insert k out left, right }

        Right out ->
          { left, right: Map.insert k out right }


toArray :: ∀ k v. Ord k => Map k v -> Array (Tuple k v)
toArray =
  Map.toUnfoldable


unionsWith :: ∀ k v. Ord k => (v -> v -> v) -> Array (Map k v) -> Map k v
unionsWith f ts =
  Array.foldl (Map.unionWith f) Map.empty ts


mapKeys :: ∀ a b v. Ord a => Ord b => (a -> b) -> Map a v -> Map b v
mapKeys f ts =
  ts
    |> toArray
    |> map (\(Tuple a k) -> Tuple (f a) k)
    |> Map.fromFoldable
