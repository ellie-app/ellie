module Extra.HttpBuilder exposing (..)

import HttpBuilder exposing (RequestBuilder)


withMaybe : (a -> RequestBuilder b -> RequestBuilder b) -> Maybe a -> RequestBuilder b -> RequestBuilder b
withMaybe extension maybeValue builder =
    case maybeValue of
        Just a ->
            extension a builder

        Nothing ->
            builder
