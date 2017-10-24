module Pages.Editor.Logs.Model exposing (Model, default)

import BoundedDeque exposing (BoundedDeque)
import Data.Ellie.Log as Log exposing (Log)


type alias Model =
    { logs : BoundedDeque Log
    , search : String
    , scrollToBottom : Bool
    }


default : Model
default =
    { logs = BoundedDeque.empty 50
    , search = ""
    , scrollToBottom = True
    }
