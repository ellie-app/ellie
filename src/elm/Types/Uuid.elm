module Types.Uuid exposing (Uuid, uuid, toString, encode, decode)

import Regex
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Shared.Utils as Utils


type Uuid
    = Uuid String


toString : Uuid -> String
toString (Uuid string) =
    string


encode : Uuid -> Value
encode =
    toString >> Encode.string


decode : Decoder Uuid
decode =
    Utils.customDecoder Decode.string uuid


uuid : String -> Result String Uuid
uuid input =
    let
        uuidRegex =
            Regex.regex "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
    in
        if Regex.contains uuidRegex input then
            Ok (Uuid input)
        else
            Err (input ++ " is not a UUID")
