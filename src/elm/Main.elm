module Main exposing (main)

import Html
import Model exposing (Model)
import Update exposing (Msg(..))
import View
import Subs


main : Program Never Model Msg
main =
    Html.program
        { view = View.view
        , update = Update.update
        , init = ( Model.model, Update.initialize )
        , subscriptions = Subs.subscriptions
        }
