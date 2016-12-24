module Types.Revision
    exposing
        ( Revision
        , encode
        , decode
        , empty
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Types.Dependency as Dependency exposing (Dependency)
import Types.Uuid as Uuid exposing (Uuid)
import Shared.Utils as Utils


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , dependencies : List Dependency
    , owned : Bool
    , revisionNumber : Maybe Int
    , projectId : Maybe Uuid
    }


empty : Revision
empty =
    { htmlCode = ""
    , elmCode = ""
    , dependencies = []
    , owned = False
    , revisionNumber = Nothing
    , projectId = Nothing
    }


encode : Revision -> Value
encode revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "dependencies", Encode.list <| List.map Dependency.encode revision.dependencies )
        , ( "owned", Encode.bool revision.owned )
        , ( "revisionNumber", Utils.encodeNullable Encode.int revision.revisionNumber )
        , ( "projectId", Utils.encodeNullable Uuid.encode revision.projectId )
        ]


decode : Decoder Revision
decode =
    Decode.decode Revision
        |> Decode.required "htmlCode" Decode.string
        |> Decode.required "elmCode" Decode.string
        |> Decode.required "dependencies" (Decode.list Dependency.decode)
        |> Decode.optional "owned" Decode.bool True
        |> Decode.required "revisionNumber" (Decode.nullable Decode.int)
        |> Decode.required "projectId" (Decode.nullable Uuid.decode)
