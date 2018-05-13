module Pages.Editor.Flags exposing (Flags, decoder)

import Data.Jwt as Jwt exposing (Jwt)
import Json.Decode as Decode exposing (Decoder)
import Pages.Editor.Types.Revision as Revision exposing (Revision)


type alias Flags =
    { token : Maybe Jwt
    , recovery : Maybe Revision
    }


decoder : Decoder Flags
decoder =
    Decode.map2 Flags
        (Decode.field "token" (Decode.nullable Jwt.decoder))
        (Decode.field "recovery" (Decode.nullable Revision.localStorageDecoder))
