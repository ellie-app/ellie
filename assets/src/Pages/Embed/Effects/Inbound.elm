port module Pages.Embed.Effects.Inbound
    exposing
        ( Inbound(..)
        , batch
        , flatten
        , map
        , none
        )

import Pages.Embed.Types.EmbedUpdate as EmbedUpdate exposing (EmbedUpdate)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)


type Inbound msg
    = EmbedUpdates RevisionId (EmbedUpdate -> msg)
    | Batch (List (Inbound msg))
    | None


batch : List (Inbound msg) -> Inbound msg
batch =
    Batch


none : Inbound msg
none =
    None


flatten : Inbound msg -> List (Inbound msg)
flatten inbound =
    case inbound of
        None ->
            []

        Batch others ->
            List.concatMap flatten others

        _ ->
            [ inbound ]


map : (a -> b) -> Inbound a -> Inbound b
map f inbound =
    case inbound of
        EmbedUpdates rid callback ->
            EmbedUpdates rid (callback >> f)

        Batch inbounds ->
            Batch <| List.map (map f) inbounds

        None ->
            None
