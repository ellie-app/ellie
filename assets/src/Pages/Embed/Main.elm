port module Pages.Embed.Main exposing (main)

import Effect.Program as Program
import Json.Decode exposing (Value)
import Pages.Embed.State.App as State
import Pages.Embed.Types.Route as Route
import Pages.Embed.Views.App as View


port inbound : (( String, Value ) -> msg) -> Sub msg


port outbound : ( String, Value ) -> Cmd msg


main : Program.Program State.Flags State.Model State.Msg
main =
    Program.program
        { subscriptions = State.subscriptions
        , update = State.update
        , init = State.init
        , view = View.view
        , flags = State.flags
        , url = Route.parse
        , route = State.RouteChanged
        , styles = View.styles
        , inbound = inbound
        , outbound = outbound
        , title = View.title
        }
