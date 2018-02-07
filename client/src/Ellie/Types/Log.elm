module Ellie.Types.Log exposing (Log, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Log =
    { tag : String
    , body : String
    }


decoder : Decoder Log
decoder =
    Decode.decode Log
        |> Decode.required "tag" Decode.string
        |> Decode.required "body" Decode.string
