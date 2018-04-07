module Pages.Editor.Flags exposing (Flags, decoder)

import Data.Jwt as Jwt exposing (Jwt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Flags =
    { token : Maybe Jwt
    }


decoder : Decoder Flags
decoder =
    Decode.succeed Flags
        |> Decode.required "token" (Decode.nullable Jwt.decoder)
