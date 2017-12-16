module Control.Monad.Task
  ( Task
  , fromEffFnTask
  , liftEff
  , EffFnTask
  , FfiHelpers
  , ffiHelpers
  , fromMaybe
  , fromEither
  , fromExcept
  , Thread
  , fork
  , join
  , makeEmptyVar
  , readVar
  , putVar
  , takeVar
  , runAndCatch
  , runAndIgnore
  , onError
  , module Control.Monad.Eff
  , module Control.Monad.Aff.AVar
  ) where

import Ellie.Prelude

import Control.Monad.Aff (Aff, Fiber, Error, throwError)
import Control.Monad.Aff (forkAff, joinFiber, launchAff_, runAff_) as Aff
import Control.Monad.Aff.AVar (AVAR, AVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class as Eff
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (Except, ExceptT(..), runExcept, runExceptT)
import Control.Monad.Except as Except
import Control.Monad.Rec.Class (class MonadRec)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Partial.Unsafe as Partial

newtype Thread e x a =
  Thread (Fiber e (Either x a))



fork :: ∀ e x a. Task e x a -> Task e x (Thread e x a)
fork (Task inner) =
  inner
    |> runExceptT
    |> Aff.forkAff
    |> map (Thread >>> Right)
    |> ExceptT
    |> Task


join :: ∀ e x a. Thread e x a -> Task e x a
join (Thread fiber) =
  fiber
    |> Aff.joinFiber
    |> ExceptT
    |> Task


makeEmptyVar :: ∀ e x a. Task (avar :: AVAR | e) x (AVar a)
makeEmptyVar =
  Task <| Aff.liftAff <| AVar.makeEmptyVar


readVar :: ∀ e x a. AVar a -> Task (avar :: AVAR | e) x a
readVar avar =
  Task <| Aff.liftAff <| AVar.readVar avar


takeVar :: ∀ e x a. AVar a -> Task (avar :: AVAR | e) x a
takeVar avar =
  Task <| Aff.liftAff <| AVar.takeVar avar


putVar :: ∀ e x a. a -> AVar a -> Task (avar :: AVAR | e) x Unit
putVar value avar =
  Task <| Aff.liftAff <| AVar.putVar value avar


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
  }


type EffFnTask e x a =
  EffFnAff e (Either x a)


newtype Task e x a =
  Task (ExceptT x (Aff e) a)


derive newtype instance functorTask :: Functor (Task e x)
derive newtype instance applyTask :: Apply (Task e x)
derive newtype instance applicativeTask :: Applicative (Task e x)
derive newtype instance bindTask :: Bind (Task e x)
derive newtype instance monadTask :: Monad (Task e x)
derive newtype instance monadErrorTask :: MonadError x (Task e x)
derive newtype instance monadThrowTask :: MonadThrow x (Task e x)
derive newtype instance monadRecTask :: MonadRec (Task e x)


instance bifunctorTask :: Bifunctor (Task e) where
  bimap l r (Task exceptT) =
    exceptT
      |> Except.withExceptT l
      |> map r
      |> Task


fromEffFnTask :: ∀ e x a. EffFnAff e (Either x a) -> Task e x a
fromEffFnTask fn = do
  Task <| ExceptT <| Aff.fromEffFnAff fn


liftEff :: ∀ e x a. Eff e a -> Task e x a
liftEff eff =
  Task <| Aff.liftAff <| Eff.liftEff eff


fromMaybe :: ∀ e x a. x -> Maybe a -> Task e x a
fromMaybe _ (Just a) = pure a
fromMaybe x Nothing = throwError x


fromEither :: ∀ e x a. Either x a -> Task e x a
fromEither (Right a) = pure a
fromEither (Left x) = throwError x


fromExcept :: ∀ e x a. Except x a -> Task e x a
fromExcept except =
  fromEither <| runExcept <| except


runAndCatch :: ∀ e x a. (Either Error (Either x a) -> Eff e Unit) -> Task e x a -> Eff e Unit
runAndCatch onComplete (Task inner) =
  inner
    |> runExceptT
    |> Aff.runAff_ onComplete


runAndIgnore :: ∀ e x a. Task e x a -> Eff e Unit
runAndIgnore (Task inner) =
  inner
    |> runExceptT
    |> Aff.launchAff_


onError :: ∀ e x y a b. (x -> Task e x a) -> Task e x a -> Task e x a
onError handler (Task inner) =
  inner
    |> runExceptT
    |> Aff.liftAff
    |> Task
    >>=
        (\e -> case e of
          Right a -> pure a
          Left x -> handler x
        )
