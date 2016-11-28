module LoadState
    exposing
        ( LoadState(..)
        , fromResult
        , toMaybe
        , map
        , map2
        , map3
        , map4
        , map5
        , mapError
        , withDefault
        , isLoading
        , isSuccess
        )


type LoadState x a
    = Initial
    | Loading
    | Success a
    | Failure x


fromResult : Result x a -> LoadState x a
fromResult result =
    case result of
        Ok a ->
            Success a

        Err x ->
            Failure x


map : (a -> b) -> LoadState x a -> LoadState x b
map tagger loadState =
    case loadState of
        Success a ->
            Success <| tagger a

        Failure x ->
            Failure x

        Loading ->
            Loading

        Initial ->
            Initial


map2 :
    (a -> b -> c)
    -> LoadState x a
    -> LoadState x b
    -> LoadState x c
map2 tagger one two =
    Success tagger
        |> andMap one
        |> andMap two


map3 :
    (a -> b -> c -> d)
    -> LoadState x a
    -> LoadState x b
    -> LoadState x c
    -> LoadState x d
map3 tagger one two three =
    Success tagger
        |> andMap one
        |> andMap two
        |> andMap three


map4 :
    (a -> b -> c -> d -> e)
    -> LoadState x a
    -> LoadState x b
    -> LoadState x c
    -> LoadState x d
    -> LoadState x e
map4 tagger one two three four =
    Success tagger
        |> andMap one
        |> andMap two
        |> andMap three
        |> andMap four


map5 :
    (a -> b -> c -> d -> e -> f)
    -> LoadState x a
    -> LoadState x b
    -> LoadState x c
    -> LoadState x d
    -> LoadState x e
    -> LoadState x f
map5 tagger one two three four five =
    Success tagger
        |> andMap one
        |> andMap two
        |> andMap three
        |> andMap four
        |> andMap five


mapError : (x -> y) -> LoadState x a -> LoadState y a
mapError tagger loadState =
    case loadState of
        Success a ->
            Success a

        Failure x ->
            Failure <| tagger x

        Loading ->
            Loading

        Initial ->
            Initial


andMap : LoadState x a -> LoadState x (a -> b) -> LoadState x b
andMap loadState taggerState =
    case ( taggerState, loadState ) of
        ( Success tagger, Success a ) ->
            Success <| tagger a

        ( _, Failure x ) ->
            Failure x

        ( _, Loading ) ->
            Loading

        ( _, _ ) ->
            Initial


isSuccess : LoadState x a -> Bool
isSuccess loadState =
    case loadState of
        Success a ->
            True

        _ ->
            False


isLoading : LoadState x a -> Bool
isLoading loadState =
    case loadState of
        Loading ->
            True

        _ ->
            False


withDefault : a -> LoadState x a -> a
withDefault default state =
    case state of
        Success a ->
            a

        _ ->
            default


toMaybe : LoadState x a -> Maybe a
toMaybe state =
    state
        |> map Just
        |> withDefault Nothing
