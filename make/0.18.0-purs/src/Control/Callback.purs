module Control.Callback
  ( run
  , CALLBACK
  , Callback
  ) where

import Ellie.Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Foreign (Foreign)

foreign import data CALLBACK :: Effect
foreign import data Callback :: Type
foreign import _runCallback :: ∀ e. Fn3 Unit Callback Foreign (Eff (callback :: CALLBACK | e) Unit)


run :: ∀ e. Callback -> Foreign -> Eff (callback :: CALLBACK | e) Unit
run =
  runFn3 _runCallback unit
