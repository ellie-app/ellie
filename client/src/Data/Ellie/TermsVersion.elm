module Data.Ellie.TermsVersion
    exposing
        ( TermsVersion
        , decoder
        , encoder
        , fromInt
        , link
        , toString
        )

import Extra.String as String
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type TermsVersion
    = TermsVersion Int


link : TermsVersion -> String
link termsVersion =
    "/a/terms/" ++ toString termsVersion


decoder : Decoder TermsVersion
decoder =
    Decode.map TermsVersion Decode.int


encoder : TermsVersion -> Value
encoder (TermsVersion version) =
    Encode.int version


fromInt : Int -> TermsVersion
fromInt =
    TermsVersion


toString : TermsVersion -> String
toString (TermsVersion version) =
    String.fromInt version
