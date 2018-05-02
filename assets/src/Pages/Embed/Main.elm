port module Pages.Embed.Main exposing (main)

import Effect.Program as Program
import Json.Decode exposing (Value)
import Pages.Embed.State.App as AppState
import Pages.Embed.Types.Route as Route
import Pages.Embed.Views.App as AppView


port inbound : (( String, Value ) -> msg) -> Sub msg


port outbound : ( String, Value ) -> Cmd msg


main : Program.Program AppState.Model AppState.Msg
main =
    Program.program
        { subscriptions = AppState.subscriptions
        , update = AppState.update
        , init = AppState.init
        , view = AppView.view
        , flags = AppState.flags
        , url = Route.parse
        , route = AppState.RouteChanged
        , styles = AppView.styles
        , inbound = inbound
        , outbound = outbound
        }
