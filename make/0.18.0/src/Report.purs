module Report where

import Ellie.Prelude
import Control.Monad.Task (Task)
import Control.Monad.Task as Task
import Control.Callback (Callback, CALLBACK)
import Control.Callback as Callback
import Data.Blob (Blob)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, put)
import Elm.Compiler.Error as Error
import Control.Monad.Eff.Exception as Exception


data Stage
  = LoadingCompiler Number
  | InstallingPackages
  | PlanningBuild
  | Compiling { total :: Int, complete :: Int }
  | GeneratingCode
  | Success Blob
  | FinishedWithErrors (Array Error.Error)
  | Failed String


instance foreignableStage :: Foreignable Stage where
  get input =
    Foreign.fail (Foreign.ForeignError "Cannot get from Foreign for Report.Stage")

  put stage =
    case stage of
      LoadingCompiler percentage ->
        Foreign.toForeign { "type": "LoadingCompiler", percentage }

      InstallingPackages ->
        Foreign.toForeign { "type": "InstallingPackages" }

      PlanningBuild ->
        Foreign.toForeign { "type": "PlanningBuild" }

      Compiling { total, complete } ->
        Foreign.toForeign
          { "type": "Compiling"
          , total
          , complete
          }

      GeneratingCode ->
        Foreign.toForeign { "type": "GeneratingCode" }

      Success blob ->
        Foreign.toForeign
          { "type": "Success"
          , blob
          }

      FinishedWithErrors errors ->
        Foreign.toForeign
          { "type": "FinishedWithErrors"
          , errors: put errors
          }

      Failed message ->
        Foreign.toForeign
          { "type": "Failed"
          , message
          }

type Reporter =
  ∀ e. Stage -> Task (callback :: CALLBACK | e) Exception.Error Unit

reporter :: Callback -> Reporter
reporter callback stage =
  stage
    |> put
    |> Callback.run callback
    |> Task.liftEff "Reporter.report"
