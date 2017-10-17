module Data.Map.Extra
  ( mapEither
  , mapEitherWithKey
  , (!)
  , unsafeLookupFlipped
  , traverseWithKey
  )
  where

import Prelude
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Array as Array
import Partial.Unsafe as Partial
import Data.Maybe(Maybe(..))
import Data.Traversable (traverse)

infixl 9 unsafeLookupFlipped as !

unsafeLookupFlipped :: forall k v. Show k => Ord k => Map k v -> k -> v
unsafeLookupFlipped map key =
  case Map.lookup key map of
    Just value ->
      value
    Nothing ->
      Partial.unsafeCrashWith $ "tried to unsafely look up key `" <> show key <> "` in a Map but did not find it."


traverseWithKey :: forall t k a b. Ord k => Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey f map =
  map
    # (Map.toUnfoldable :: Map k a -> Array (Tuple k a))
    # traverse (\(Tuple k v) -> Tuple k <$> f k v)
    <#> Map.fromFoldable


mapEither :: forall k a b c. Ord k => (a -> Either b c) -> Map k a -> Tuple (Map k b) (Map k c)
mapEither f map =
    mapEitherWithKey (\_ v -> f v) map

mapEitherWithKey :: forall k a b c. Ord k => (k -> a -> Either b c) -> Map k a -> Tuple (Map k b) (Map k c)
mapEitherWithKey f map =
  map
    # (Map.toUnfoldable :: Map k a -> Array (Tuple k a))
    # Array.foldr fold (Tuple Map.empty Map.empty)
  where
    fold (Tuple k v) (Tuple l r) =
      case f k v of
          Left out ->
              Tuple (Map.insert k out l) r

          Right out ->
              Tuple l (Map.insert k out r)
