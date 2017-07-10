module Pages.Embed.Main exposing (main)

import Navigation
import Pages.Embed.Model as Model exposing (Model)
import Pages.Embed.Update as Update exposing (Msg(..))
import Pages.Embed.View as View
import Pages.Embed.Subs as Subs


main : Program Never Model Msg
main =
    Navigation.program Update.onRouteChange
        { view = View.view
        , update = Update.update
        , subscriptions = Subs.subscriptions
        , init = Update.initialize
        }
