module Pages.Editor.Main exposing (main)

import Pages.Editor.Effects.Program as Program
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Route as Route
import Pages.Editor.State.App as AppState
import Pages.Editor.View as View


main : Program.EffectsProgram AppState.Model AppState.Msg
main =
    Program.program
        { subscriptions = AppState.subscriptions
        , update = AppState.update
        , init = AppState.init
        , view = View.view
        , error = AppState.ExceptionOccurred
        , flags = Flags.decoder
        , url = Route.parse
        , route = AppState.RouteChanged
        , styles = View.styles
        }
