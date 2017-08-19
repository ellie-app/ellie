module Data.Ellie.TermsVersion
    exposing
        ( TermsVersion
        , decoder
        , encoder
        , fromInt
        , toString
        )

import Extra.String as String
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type TermsVersion
    = TermsVersion Int


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
