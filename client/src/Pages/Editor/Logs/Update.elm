module Pages.Editor.Logs.Update exposing (Msg(..), update)

import Data.Ellie.Log as Log exposing (Log)
import Pages.Editor.Logs.Model as Model exposing (Model)


type Msg
    = LogReceived Log
    | SearchChanged String
    | LogsCleared
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        LogReceived log ->
            { model | logs = log :: model.logs }

        SearchChanged search ->
            { model | search = search }

        LogsCleared ->
            { model | logs = [] }

        NoOp ->
            model
