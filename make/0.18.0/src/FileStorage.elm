module FileStorage exposing (exists, isOlderThan, modified, read, write)

import Data.FilePath as FilePath exposing (FilePath)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Native.Storage
import Task exposing (Task)
import Time exposing (Time)


valueDecoder : Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.field "value" Decode.value
        , Decode.null Encode.null
        ]


modifiedDecoder : Decoder (Maybe Time)
modifiedDecoder =
    Decode.oneOf
        [ Decode.map Just <| Decode.field "modified" Decode.float
        , Decode.null Nothing
        ]


read : FilePath -> Task String Value
read path =
    path
        |> FilePath.resolve
        |> Native.Storage.get
        |> Task.map
            (\value ->
                value
                    |> Decode.decodeValue valueDecoder
                    |> Result.withDefault Encode.null
            )


modified : FilePath -> Task String (Maybe Time)
modified path =
    path
        |> FilePath.resolve
        |> Native.Storage.get
        |> Task.map
            (\value ->
                value
                    |> Decode.decodeValue modifiedDecoder
                    |> Result.withDefault Nothing
            )


write : FilePath -> Value -> Task String Value
write path value =
    Time.now
        |> Task.map (\time -> Encode.object [ ( "value", value ), ( "modified", Encode.float time ) ])
        |> Task.andThen (Native.Storage.set (FilePath.resolve path))
        |> Task.map (\_ -> value)


exists : FilePath -> Task String Bool
exists path =
    path
        |> FilePath.resolve
        |> Native.Storage.get
        |> Task.map
            (\result ->
                result
                    |> Decode.decodeValue (Decode.null False)
                    |> Result.withDefault True
            )


negativeInfinity : Time
negativeInfinity =
    -1 / 0


isOlderThan : Time -> FilePath -> Task String Bool
isOlderThan duration path =
    Task.map2 (,)
        Time.now
        (modified path |> Task.map (Maybe.withDefault negativeInfinity))
        |> Task.map (\( now, lastModified ) -> (now - lastModified) > duration)
