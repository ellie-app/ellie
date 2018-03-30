module Data.Replaceable exposing (..)

import Data.Entity as Entity exposing (Entity)


type Replaceable k v
    = NotAsked
    | Loading k
    | Replacing k (Entity k v)
    | Loaded (Entity k v)


fromMaybe : Maybe (Entity k v) -> Replaceable k v
fromMaybe maybeEntity =
    maybeEntity
        |> Maybe.map Loaded
        |> Maybe.withDefault NotAsked


toMaybe : Replaceable k v -> Maybe (Entity k v)
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


inject : Entity k v -> Replaceable k v -> Replaceable k v
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
