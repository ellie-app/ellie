module Extra.Result exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


decoder : Decoder x -> Decoder a -> Decoder (Result x a)
decoder x a =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "Ok" ->
                        Decode.field "arg" a
                            |> Decode.map Ok

                    "Err" ->
                        Decode.field "arg" x
                            |> Decode.map Err

                    _ ->
                        Decode.fail ("Unknown Result constructor " ++ tipe)
            )
