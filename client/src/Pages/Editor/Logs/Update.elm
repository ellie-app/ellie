module Pages.Editor.Logs.Update exposing (Msg(..), update)

import BoundedDeque
import Data.Ellie.Log as Log exposing (Log)
import Dom.Scroll as Scroll
import Extra.BoundedDeque as BoundedDeque
import Pages.Editor.Logs.Model as Model exposing (Model)
import Task


type Msg
    = LogReceived Log
    | SearchChanged String
    | LogsCleared
    | UpdateScrollState Bool
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateScrollState isAtBottom ->
            ( { model | scrollToBottom = isAtBottom }
            , Cmd.none
            )

        LogReceived log ->
            ( { model | logs = BoundedDeque.pushBack log model.logs }
            , if model.scrollToBottom then
                Task.attempt (\_ -> NoOp) (Scroll.toBottom "pagesEditorLogsLogs")
              else
                Cmd.none
            )

        SearchChanged search ->
            ( { model | search = search }
            , Cmd.none
            )

        LogsCleared ->
            ( { model | logs = BoundedDeque.clear model.logs }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )
