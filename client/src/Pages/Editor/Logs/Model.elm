module Pages.Editor.Logs.Model exposing (Model, model)

import BoundedDeque exposing (BoundedDeque)
import Data.Ellie.Log as Log exposing (Log)


type alias Model =
    { logs : BoundedDeque Log
    , search : String
    , scrollToBottom : Bool
    }


model : Model
model =
    { logs = BoundedDeque.empty 50
    , search = ""
    , scrollToBottom = True
    }
