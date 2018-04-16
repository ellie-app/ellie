port module Pages.Embed.Effects.Inbound
    exposing
        ( Inbound(..)
        , batch
        , flatten
        , map
        , none
        )


type Inbound msg
    = Batch (List (Inbound msg))
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


map : (a -> b) -> Inbound a -> Inbound b
map f inbound =
    case inbound of
        Batch inbounds ->
            Batch <| List.map (map f) inbounds

        None ->
            None
