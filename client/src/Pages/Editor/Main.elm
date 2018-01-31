module Pages.Editor.Main exposing (main)

import Ellie.Effect.Program as Program
import Json.Decode as Decode exposing (Value)
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Route as Route
import Pages.Editor.Update as Update exposing (Msg(..))
import Pages.Editor.View as View


main : Program Value Model Msg
main =
    Program.program
        { subscriptions = Update.subscriptions
        , update = Update.update
        , start = AppStart
        , pass = NoOp
        , error = ErrorOccured
        , flags = Flags.decoder
        , view = View.view
        , url = Route.parse
        , route = RouteChanged
        , model = Model.model
        , styles = View.styles
        }
