module Types.NewRevision exposing (..)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Types.Dependency as Dependency exposing (Dependency)


type alias NewRevision =
    { htmlCode : String
    , elmCode : String
    , dependencies : List Dependency
    }


encode : NewRevision -> Value
encode revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "dependencies", Encode.list <| List.map Dependency.encode revision.dependencies )
        ]


decode : Decoder NewRevision
decode =
    Decode.decode NewRevision
        |> Decode.required "htmlCode" Decode.string
        |> Decode.required "elmCode" Decode.string
        |> Decode.required "dependencies" (Decode.list Dependency.decode)
