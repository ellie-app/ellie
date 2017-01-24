module Apps.Editor.Main exposing (main)

import Navigation
import Apps.Editor.Model as Model exposing (Model, Flags)
import Apps.Editor.Update as Update exposing (Msg(..))
import Apps.Editor.View as View
import Apps.Editor.Subs as Subs


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Update.onRouteChange
        { view = View.view
        , update = Update.update
        , subscriptions = Subs.subscriptions
        , init = Update.initialize
        }
