module Data.Replaceable exposing (..)

import Data.Entity exposing (Entity)


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
