module Pages.Embed.Subs exposing (subscriptions)

import Pages.Embed.Model as Model exposing (Model)
import Pages.Embed.Update as Update exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
