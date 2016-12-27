module Main exposing (main)

import Navigation
import App.Model as Model exposing (Model, Flags)
import App.Update as Update exposing (Msg(..))
import App.View as View
import App.Subs as Subs


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Update.onRouteChange
        { view = View.view
        , update = Update.update
        , subscriptions = Subs.subscriptions
        , init = Update.initialize
        }
