module Pages.Embed.Main exposing (main)

import Pages.Embed.Effects.Program as Program
import Pages.Embed.State.App as AppState
import Pages.Embed.Types.Route as Route
import Pages.Embed.Views.App as AppView


main : Program.EffectsProgram AppState.Model AppState.Msg
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
        }
