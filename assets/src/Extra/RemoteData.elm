module Extra.RemoteData exposing (..)

import RemoteData exposing (RemoteData)


fromMaybe : Maybe a -> RemoteData x a
fromMaybe maybeValue =
    case maybeValue of
        Just a ->
            RemoteData.Success a

        Nothing ->
            RemoteData.NotAsked
