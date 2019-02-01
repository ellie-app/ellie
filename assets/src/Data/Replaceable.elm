module Data.Replaceable exposing (Replaceable(..), fromMaybe, inject, loading, reset, toMaybe)


type Replaceable k v
    = NotAsked
    | Loading k
    | Replacing k ( k, v )
    | Loaded ( k, v )


fromMaybe : Maybe ( k, v ) -> Replaceable k v
fromMaybe maybeEntity =
    maybeEntity
        |> Maybe.map Loaded
        |> Maybe.withDefault NotAsked


toMaybe : Replaceable k v -> Maybe ( k, v )
toMaybe replaceable =
    case replaceable of
        NotAsked ->
            Nothing

        Loading _ ->
            Nothing

        Replacing _ entity ->
            Just entity

        Loaded entity ->
            Just entity


reset : Replaceable k v -> Replaceable k v
reset replaceable =
    case replaceable of
        Replacing _ entity ->
            Loaded entity

        Loaded entity ->
            Loaded entity

        _ ->
            NotAsked


loading : Replaceable k v -> Maybe k
loading replaceable =
    case replaceable of
        NotAsked ->
            Nothing

        Loading k ->
            Just k

        Replacing k _ ->
            Just k

        Loaded entity ->
            Nothing


inject : ( k, v ) -> Replaceable k v -> Replaceable k v
inject entity replaceable =
    case replaceable of
        NotAsked ->
            Loaded entity

        Loading k ->
            Replacing k entity

        Replacing k _ ->
            Replacing k entity

        Loaded _ ->
            Loaded entity
