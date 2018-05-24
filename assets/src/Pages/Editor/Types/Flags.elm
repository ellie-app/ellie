module Pages.Editor.Types.Flags exposing (Flags, decoder)

import Data.Jwt as Jwt exposing (Jwt)
import Json.Decode as Decode exposing (Decoder)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Types.User as User exposing (User)


type alias Flags =
    { recovery : Maybe Revision
    , user : Maybe User
    , latestTerms : Int
    }


decoder : Decoder Flags
decoder =
    Decode.map3 Flags
        (Decode.field "recovery" (Decode.nullable Revision.localStorageDecoder))
        (Decode.field "user" (Decode.nullable User.localStorageDecoder))
        (Decode.field "latestTerms" Decode.int)
