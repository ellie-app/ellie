module Pages.Editor.Logs.Model exposing (Model, default)

import Data.Ellie.Log as Log exposing (Log)


type alias Model =
    { logs : List Log
    , search : String
    }


default : Model
default =
    { logs = []
    , search = ""
    }
