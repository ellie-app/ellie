module Elm.Package.Version exposing (Version, fromString, compare, toString, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


fromString : String -> Maybe Version
fromString str =
    case String.split "." str of
        [ major, minor, patch ] ->
            Result.toMaybe <|
                Result.map3
                    Version
                    (String.toInt major)
                    (String.toInt minor)
                    (String.toInt patch)

        _ ->
            Nothing


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
                Just version ->
                    Decode.succeed version

                Nothing ->
                    Decode.fail "a version like \"2.0.3\" or \"4.2.1\""
    in
        Decode.andThen check Decode.string


encoder : Version -> Value
encoder version =
    Encode.string <| toString version
