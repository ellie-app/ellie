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


eq : (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eq inner left right =
    case ( left, right ) of
        ( Just l, Just r ) ->
            inner l r

        ( Nothing, Nothing ) ->
            True

        _ ->
            False
