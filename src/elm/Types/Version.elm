module Types.Version exposing (..)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)


type alias Version =
    { major : Int
    , minor : Int
    , patch : Int
    }


parse : String -> Maybe Version
parse string =
    let
        parseInt =
            String.toInt >> Result.toMaybe

        split =
            String.split "." string

        major =
            split |> List.head |> Maybe.andThen parseInt

        minor =
            split |> List.drop 1 |> List.head |> Maybe.andThen parseInt

        patch =
            split |> List.drop 2 |> List.head |> Maybe.andThen parseInt
    in
        Just Version
            |> Maybe.map2 (|>) major
            |> Maybe.map2 (|>) minor
            |> Maybe.map2 (|>) patch


compare : Version -> Version -> Order
compare left right =
    if left.major == right.major then
        if left.minor == right.minor then
            Basics.compare left.patch right.patch
        else
            Basics.compare left.minor right.minor
    else
        Basics.compare left.major right.major


nextMajor : Version -> Version
nextMajor version =
    Version (version.major + 1) 0 0


toString : Version -> String
toString version =
    (Basics.toString version.major)
        ++ "."
        ++ (Basics.toString version.minor)
        ++ "."
        ++ (Basics.toString version.patch)


encode : Version -> Value
encode version =
    Encode.object
        [ ( "major", Encode.int version.major )
        , ( "minor", Encode.int version.minor )
        , ( "patch", Encode.int version.patch )
        ]


decode : Decoder Version
decode =
    Decode.string
        |> Decode.andThen
            (\string ->
                case parse string of
                    Just version ->
                        Decode.succeed version

                    Nothing ->
                        Decode.fail ("Could not parse version " ++ string)
            )
