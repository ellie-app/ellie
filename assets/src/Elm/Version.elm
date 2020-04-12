module Elm.Version exposing (Version, compare, compatible, decoder, encoder, eq, fromString, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


fromString : String -> Result String Version
fromString str =
    case String.split "." str of
        [ major, minor, patch ] ->
            Result.fromMaybe "Expecting a version as <Int>.<Int>.<Int>" <|
                Maybe.map3 Version
                    (String.toInt major)
                    (String.toInt minor)
                    (String.toInt patch)

        _ ->
            Err "Expecting a version like MAJOR.MINOR.PATCH"


compatible : Version -> Version -> Bool
compatible left right =
    ( left.major, left.minor ) == ( right.major, right.minor )


eq : Version -> Version -> Bool
eq left right =
    case compare left right of
        EQ ->
            True

        _ ->
            False


compare : Version -> Version -> Order
compare left right =
    if left.major == right.major then
        if left.minor == right.minor then
            Basics.compare left.patch right.patch

        else
            Basics.compare left.minor right.minor

    else
        Basics.compare left.major right.major


toString : Version -> String
toString version =
    String.fromInt version.major
        ++ "."
        ++ String.fromInt version.minor
        ++ "."
        ++ String.fromInt version.patch


decoder : Decode.Decoder Version
decoder =
    let
        check str =
            case fromString str of
                Ok version ->
                    Decode.succeed version

                Err message ->
                    Decode.fail message
    in
    Decode.andThen check Decode.string


encoder : Version -> Value
encoder version =
    Encode.string <| toString version
