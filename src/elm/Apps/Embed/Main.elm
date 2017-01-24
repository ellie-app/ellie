module Apps.Embed.Main exposing (main)

import Navigation
import Apps.Embed.Model as Model exposing (Model)
import Apps.Embed.Update as Update exposing (Msg(..))
import Apps.Embed.View as View
import Apps.Embed.Subs as Subs


main : Program Never Model Msg
main =
    Navigation.program Update.onRouteChange
        { view = View.view
        , update = Update.update
        , subscriptions = Subs.subscriptions
        , init = Update.initialize
        }
