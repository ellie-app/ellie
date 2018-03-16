module Pages.Editor.Types.TermsVersion
    exposing
        ( TermsVersion
        , compare
        , decoder
        , encoder
        , eq
        )

import Data.Url as Url exposing (Url)
import Extra.String as String
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type TermsVersion
    = TermsVersion Int


decoder : Decoder TermsVersion
decoder =
    Decode.map TermsVersion Decode.int


encoder : TermsVersion -> Value
encoder (TermsVersion int) =
    Encode.int int


eq : TermsVersion -> TermsVersion -> Bool
eq (TermsVersion left) (TermsVersion right) =
    left == right


compare : TermsVersion -> TermsVersion -> Order
compare (TermsVersion left) (TermsVersion right) =
    Basics.compare left right


link : TermsVersion -> Url
link (TermsVersion int) =
    Url.fromString <| "/a/terms/" ++ String.fromInt int
