module Types.PackageSearchResult
    exposing
        ( PackageSearchResult
        , encode
        , decode
        , toString
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode


type alias PackageSearchResult =
    { username : String
    , name : String
    }


toString : PackageSearchResult -> String
toString package =
    package.username ++ "/" ++ package.name


encode : PackageSearchResult -> Value
encode package =
    Encode.object
        [ ( "username", Encode.string package.username )
        , ( "name", Encode.string package.name )
        ]


decode : Decoder PackageSearchResult
decode =
    Decode.decode PackageSearchResult
        |> Decode.required "username" Decode.string
        |> Decode.required "name" Decode.string
