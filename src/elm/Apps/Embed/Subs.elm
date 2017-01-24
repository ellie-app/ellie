port module Apps.Embed.Subs exposing (subscriptions)

import Apps.Embed.Model as Model exposing (Model)
import Apps.Embed.Update as Update exposing (Msg(..))


port windowUnloadedIn : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
