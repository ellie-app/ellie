module Control.Monad.Task
  ( Task
  , Process
  , EffFnTask
  , FfiHelpers
  , fork
  , finally
  , kill
  , liftEff
  , fromEffFnTask
  , ffiHelpers
  , fromEither
  , fromExcept
  , fromMaybe
  , makeEmptyVar
  , takeVar
  , readVar
  , putVar
  , module Control.Monad.Eff
  , module Control.Monad.Eff.AVar
  ) where

import Ellie.Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.AVar (AVAR, AVar)
import Control.Monad.Eff.AVar as AVar
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Partial.Unsafe as Partial


foreign import data Process :: # Effect -> Type -> Type -> Type
foreign import data Task :: # Effect -> Type -> Type -> Type
foreign import data EffFnTask :: # Effect -> Type -> Type -> Type
foreign import _succeed :: ∀ e x a. a -> Task e x a
foreign import _fail :: ∀ e x a. x -> Task e x a
foreign import _bind :: ∀ e x a b. Fn2 (a -> Task e x b) (Task e x a) (Task e x b)
foreign import _fromEffFnTask :: ∀ e x a. Fn2 String (EffFnTask e x a) (Task e x a)
foreign import _onError :: ∀ e x a y. Fn2 (x -> Task e y a) (Task e x a) (Task e y a)
foreign import _spawn :: ∀ e x a. Task e x a -> Eff e (Process e x a)
foreign import _kill :: ∀ e x a. Fn2 Unit (Process e x a) (Eff e Unit)
foreign import _liftEff :: ∀ e a. Fn2 String (Eff e a) (Task e Error a)
foreign import _wrapAVarOp :: ∀ e a. Fn3 FfiHelpers String ((Either Error a -> Eff (avar :: AVAR | e) Unit) -> Eff (avar :: AVAR | e) (Eff (avar :: AVAR | e) Unit)) (Task e Error a)


fromMaybe :: ∀ e x a. x -> Maybe a -> Task e x a
fromMaybe _ (Just a) = _succeed a
fromMaybe x Nothing = _fail x


fromEither :: ∀ e x. Either x ~> Task e x
fromEither (Left x) = _fail x
fromEither (Right a) = _succeed a


fromExcept :: ∀ e x. Except x ~> Task e x
fromExcept = runExcept >>> fromEither


makeEmptyVar :: ∀ e a. Task (avar ∷ AVAR | e) Error (AVar a)
makeEmptyVar = liftEff "Task.makeEmptyVar" AVar.makeEmptyVar

readVar :: ∀ e a. AVar a -> Task (avar :: AVAR | e) Error a
readVar avar =
  runFn3 _wrapAVarOp ffiHelpers "Task.readVar" (AVar.readVar avar)


takeVar :: ∀ e a. AVar a -> Task (avar :: AVAR | e) Error a
takeVar avar =
  runFn3 _wrapAVarOp ffiHelpers "Task.takeVar" (AVar.takeVar avar)


putVar ∷ ∀ e a. a -> AVar a -> Task (avar ∷ AVAR | e) Error Unit
putVar value avar = 
  runFn3 _wrapAVarOp ffiHelpers "Task.putVar" (AVar.putVar value avar)


fork ::  ∀ e x a. Task e x a -> Eff e (Process e x a)
fork = _spawn


kill ::  ∀ e x a. Process e x a -> Eff e Unit
kill = runFn2 _kill unit


fromEffFnTask :: ∀ e x a. String -> EffFnTask e x a -> Task e x a
fromEffFnTask = runFn2 _fromEffFnTask


liftEff :: ∀ e a. String -> Eff e a -> Task e Error a
liftEff = runFn2 _liftEff


finally :: ∀ e x a. Task e x Unit -> Task e x a -> Task e x a
finally cleanup actual =
  (actual >>= (\a -> cleanup $> a)) `catchError` (\x -> cleanup >>= (const (throwError x)))


instance functorTask :: Functor (Task e x) where
  map f t = bind t (f >>> pure)

instance applyTask :: Apply (Task e x) where
  apply mf m = bind mf (\f -> map f m)

instance applicativeTask :: Applicative (Task e x) where
  pure = _succeed

instance bindTask :: Bind (Task e x) where
  bind m f = runFn2 _bind f m

instance monadTask :: Monad (Task e x)

instance monadThrow :: MonadThrow x (Task e x) where
  throwError = _fail

instance monadError :: MonadError x (Task e x) where
  catchError m f = runFn2 _onError f m

instance bifunctorTask :: Bifunctor (Task e) where
  bimap l r t = runFn2 _onError (l >>> _fail) (map r t)

instance monadRecTask :: MonadRec (Task e x) where
  tailRecM k = go
    where
    go a = do
      res ← k a
      case res of
        Done r → pure r
        Loop b → go b



type FfiHelpers =
  { left ∷ ∀ a b. a -> Either a b
  , right ∷ ∀ a b. b -> Either a b
  , isLeft :: ∀ a b. Either a b -> Boolean
  , isRight :: ∀ a b. Either a b -> Boolean
  , just :: ∀ a. a -> Maybe a
  , nothing :: ∀ a. Maybe a
  , isJust :: ∀ a. Maybe a -> Boolean
  , isNothing :: ∀ a. Maybe a -> Boolean
  , unit :: Unit
  , const :: ∀ a b. b -> a -> b
  , fromJust :: ∀ a. Maybe a -> a
  , fromLeft :: ∀ x a. Either x a -> x
  , fromRight :: ∀ x a. Either x a -> a
  }


ffiHelpers :: FfiHelpers
ffiHelpers =
  { left: Left
  , right: Right
  , isLeft: Either.isLeft
  , isRight: Either.isRight
  , just: Just
  , nothing: Nothing
  , isJust: Maybe.isJust
  , isNothing: Maybe.isNothing
  , unit: unit
  , const: const
  , fromJust: Partial.unsafePartial (Maybe.fromJust)
  , fromLeft: Partial.unsafePartial (Either.fromLeft)
  , fromRight: Partial.unsafePartial (Either.fromRight)
  }
