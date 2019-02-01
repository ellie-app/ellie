module Pages.Editor.Types.Log exposing (Log, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias Log =
    { label : String
    , body : String
    }


decoder : Decoder Log
decoder =
    Decode.map2 Log
        (Decode.field "label" Decode.string)
        (Decode.field "body" Decode.string)
