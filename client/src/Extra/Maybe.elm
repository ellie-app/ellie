module Extra.Maybe exposing (..)


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy default maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            default ()
