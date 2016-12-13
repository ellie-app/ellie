module Main exposing (main)

import Html
import App.Model as Model exposing (Model)
import App.Update as Update exposing (Msg(..))
import App.View as View
import App.Subs as Subs


main : Program Never Model Msg
main =
    Html.program
        { view = View.view
        , update = Update.update
        , init = ( Model.model, Update.initialize )
        , subscriptions = Subs.subscriptions
        }
