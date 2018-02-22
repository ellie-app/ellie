module Pages.Editor.State.Workbench
    exposing
        ( Model
        , Msg(..)
        , subscriptions
        , update
        )

import BoundedDeque exposing (BoundedDeque)
import Ellie.Types.Log exposing (Log)
import Pages.Editor.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Editor.Effects.Outbound as Outbound exposing (Outbound)


type Pane
    = Output
    | Logs
    | Debug
    | Share


type alias Model =
    { logs : BoundedDeque Log
    , pane : Pane
    }


initial : Model
initial =
    { logs = BoundedDeque.empty 100
    , pane = Output
    }


type Msg
    = LogReceived Log
    | UserPressedReloadIframe
    | UserPressedClearLogs
    | UserSelectedPane Pane


update : Msg -> Model -> ( Model, Outbound Msg )
update msg model =
    case msg of
        LogReceived log ->
            ( { model | logs = BoundedDeque.pushFront log model.logs }
            , Outbound.none
            )

        UserPressedReloadIframe ->
            ( model
            , Outbound.ReloadIframe (model.pane == Debug)
            )

        UserPressedClearLogs ->
            ( { model | logs = BoundedDeque.empty 100 }
            , Outbound.none
            )

        UserSelectedPane Debug ->
            ( { model | pane = Debug }
            , Outbound.SwitchToDebugger
            )

        UserSelectedPane Output ->
            ( { model | pane = Output }
            , Outbound.SwitchToProgram
            )

        UserSelectedPane pane ->
            ( { model | pane = pane }
            , Outbound.none
            )


subscriptions : Model -> Inbound Msg
subscriptions model =
    Inbound.LogReceived LogReceived
