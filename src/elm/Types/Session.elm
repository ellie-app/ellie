module Types.Session
    exposing
        ( Session
        , decode
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Session =
    { id : String
    }


decode : Decoder Session
decode =
    Decode.succeed Session
        |> Decode.required "id" Decode.string
