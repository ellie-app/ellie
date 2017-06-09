module Data.Extra.Json.Decode exposing (..)

import Json.Decode as Decode exposing (Decoder)


optionalField : String -> a -> Decoder a -> Decoder a
optionalField fieldName default decoder =
    let
        finishDecoding json =
            case Decode.decodeValue (Decode.field fieldName Decode.value) json of
                Ok val ->
                    -- The field is present, so run the decoder on it.
                    decoder

                Err _ ->
                    -- The field was missing, which is fine!
                    Decode.succeed default
    in
        Decode.value
            |> Decode.andThen finishDecoding
