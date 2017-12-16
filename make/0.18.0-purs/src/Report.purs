module Report where

import Ellie.Prelude
import Control.Monad.Task (Task)
import Control.Callback (Callback, CALLBACK)
import Control.Callback as Callback
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, put)
import Elm.Compiler.Error as Error


data Stage
  = InstallingPackages
  | PlanningBuild
  | Compiling { total :: Int, complete :: Int }
  | GeneratingCode
  | Success String
  | FinishedWithErrors (Array Error.Error)
  | Failed String


instance foreignableStage :: Foreignable Stage where
  get input =
    Foreign.fail (Foreign.ForeignError "Cannot get from Foreign for Report.Stage")

  put stage =
    case stage of
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

      Success url ->
        Foreign.toForeign
          { "type": "Success"
          , url
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
  ∀ x e. Stage -> Task (callback :: CALLBACK | e) x Unit

reporter :: Callback -> Reporter
reporter callback stage =
  Callback.run callback <| put stage
