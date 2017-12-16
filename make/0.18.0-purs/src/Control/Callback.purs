module Control.Callback
  ( run
  , CALLBACK
  , Callback
  ) where

import Ellie.Prelude
import Control.Monad.Task (Task, EffFnTask, kind Effect)
import Control.Monad.Task as Task
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Foreign (Foreign)

foreign import data CALLBACK :: Effect

foreign import data Callback :: Type

foreign import _runCallback ::
  ∀ e x
  . Fn3
      Task.FfiHelpers
      Callback
      Foreign
      (EffFnTask (callback :: CALLBACK | e) x Unit)


run :: ∀ x e. Callback -> Foreign -> Task (callback :: CALLBACK | e) x Unit
run callback value =
  Task.fromEffFnTask <|
    runFn3 _runCallback Task.ffiHelpers callback value
