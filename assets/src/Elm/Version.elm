module Elm.Version exposing (Version, compare, decoder, encoder, fromString, toString)

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
            Result.map3
                Version
                (String.toInt major)
                (String.toInt minor)
                (String.toInt patch)

        _ ->
            Err "Expecting a version like MAJOR.MINOR.PATCH"


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
    Basics.toString version.major
        ++ "."
        ++ Basics.toString version.minor
        ++ "."
        ++ Basics.toString version.patch


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
