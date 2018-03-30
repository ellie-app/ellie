module Ellie.Types.User
    exposing
        ( Id
        , User
        , decoder
        , idDecoder
        , idEncoder
        , idEq
        )

import Ellie.Types.Settings as Settings exposing (Settings)
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type Id
    = Id String


idDecoder : Decoder Id
idDecoder =
    Decode.map Id Decode.string


idEncoder : Id -> Value
idEncoder (Id uuid) =
    Encode.string uuid


idEq : Id -> Id -> Bool
idEq (Id left) (Id right) =
    left == right


type alias User =
    { termsVersion : Maybe TermsVersion
    , settings : Settings
    }


decoder : Decoder User
decoder =
    Decode.decode User
        |> Decode.optional "termsVersion" (Decode.map Just TermsVersion.decoder) Nothing
        |> Decode.required "settings" Settings.decoder
