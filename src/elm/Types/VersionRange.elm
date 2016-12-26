module Types.VersionRange
    exposing
        ( VersionRange
        , parse
        , toString
        , includes
        , encode
        , decode
        , hash
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Types.Version as Version exposing (Version)


type alias VersionRange =
    { min : Version
    , max : Version
    }


parse : String -> Maybe VersionRange
parse string =
    let
        split =
            string |> String.split " <= v < "

        min =
            split |> List.head |> Maybe.andThen Version.parse

        max =
            split |> List.head |> Maybe.andThen Version.parse
    in
        Just VersionRange
            |> (Maybe.map2 (|>) min)
            |> (Maybe.map2 (|>) max)


toString : VersionRange -> String
toString versionRange =
    (Version.toString versionRange.min)
        ++ " <= v < "
        ++ (Version.toString versionRange.max)


includes : Version -> VersionRange -> Bool
includes version versionRange =
    (Version.compare version versionRange.min /= LT)
        && (Version.compare version versionRange.max == LT)


encode : VersionRange -> Value
encode versionRange =
    Encode.object
        [ ( "min", Version.encode versionRange.min )
        , ( "max", Version.encode versionRange.max )
        ]


decodeString : Decoder VersionRange
decodeString =
    Decode.string
        |> Decode.andThen
            (\string ->
                case parse string of
                    Just versionRange ->
                        Decode.succeed versionRange

                    Nothing ->
                        Decode.fail ("Could not parse version range " ++ string)
            )


decodeJson : Decoder VersionRange
decodeJson =
    Decode.decode VersionRange
        |> Decode.required "min" Version.decode
        |> Decode.required "max" Version.decode


decode : Decoder VersionRange
decode =
    Decode.oneOf
        [ decodeString
        , decodeJson
        ]


hash : VersionRange -> String
hash versionRange =
    Version.hash versionRange.min
        ++ "<=v<"
        ++ Version.hash versionRange.max
