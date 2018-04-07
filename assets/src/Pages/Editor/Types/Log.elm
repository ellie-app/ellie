module Pages.Editor.Types.Log exposing (Log, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Log =
    { label : String
    , body : String
    }


decoder : Decoder Log
decoder =
    Decode.decode Log
        |> Decode.required "label" Decode.string
        |> Decode.required "body" Decode.string
