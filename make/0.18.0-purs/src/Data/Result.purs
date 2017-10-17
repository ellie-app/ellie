module Data.Result where

import Data.Bifunctor
import Prelude
import Data.Maybe


data Result x a
  = Ok a
  | Err x


instance functorResult :: Functor (Result x) where
  map f (Ok a) = Ok (f a)
  map _ (Err x) = Err x

instance bifunctorResult :: Bifunctor Result where
  bimap f _ (Err l) = Err (f l)
  bimap _ g (Ok r) = Ok (g r)

instance bindResult :: Bind (Result x) where
  bind (Ok a) f = f a
  bind (Err x) f = Err x

instance applyResult :: Apply (Result x) where
  apply (Err e) _ = Err e
  apply (Ok f) r = f <$> r

instance applicativeResult :: Applicative (Result x) where
  pure = Ok

instance monadResult :: Monad (Result x)

liftMaybe :: forall x a. x -> Maybe a -> Result x a
liftMaybe err maybe =
  case maybe of
    Just a -> Ok a
    Nothing -> Err err
