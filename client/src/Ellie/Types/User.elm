module Ellie.Types.User
    exposing
        ( Id
        , User
        , decoder
        )

import Ellie.Types.Settings as Settings exposing (Settings)
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias Id =
    String


type alias User =
    { termsVersion : Maybe TermsVersion
    , settings : Settings
    }


decoder : Decoder User
decoder =
    Decode.decode User
        |> Decode.optional "termsVersion" (Decode.map Just TermsVersion.decoder) Nothing
        |> Decode.required "settings" Settings.decoder
