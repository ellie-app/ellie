module Types.Dependency
    exposing
        ( Dependency
        , encode
        , decode
        , toString
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Types.VersionRange as VersionRange exposing (VersionRange)


type alias Dependency =
    { username : String
    , name : String
    , range : VersionRange
    }


toString : Dependency -> String
toString dependency =
    dependency.username
        ++ "/"
        ++ dependency.name
        ++ " @ "
        ++ (VersionRange.toString dependency.range)


encode : Dependency -> Value
encode dependency =
    Encode.object
        [ ( "username", Encode.string dependency.username )
        , ( "name", Encode.string dependency.name )
        , ( "range", VersionRange.encode dependency.range )
        ]


decode : Decoder Dependency
decode =
    Decode.decode Dependency
        |> Decode.required "username" Decode.string
        |> Decode.required "name" Decode.string
        |> Decode.required "range" VersionRange.decode
