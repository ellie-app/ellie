port module Pages.Editor.Main exposing (main)

import Effect.Program as Program
import Json.Decode exposing (Value)
import Pages.Editor.Route as Route
import Pages.Editor.State.App as AppState
import Pages.Editor.Types.Flags as Flags exposing (Flags)
import Pages.Editor.View as View


port outbound : ( String, Value ) -> Cmd msg


port inbound : (( String, Value ) -> msg) -> Sub msg


main : Program.Program Flags.Flags AppState.Model AppState.Msg
main =
    Program.program
        { subscriptions = AppState.subscriptions
        , update = AppState.update
        , init = AppState.init
        , view = View.view
        , flags = Flags.decoder
        , url = Route.parse
        , route = AppState.RouteChanged
        , styles = View.styles
        , inbound = inbound
        , outbound = outbound
        }
