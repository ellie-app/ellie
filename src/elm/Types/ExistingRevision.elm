module Types.ExistingRevision
    exposing
        ( ExistingRevision
        , encode
        , decode
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Types.Dependency as Dependency exposing (Dependency)
import Types.Uuid as Uuid exposing (Uuid)


type alias ExistingRevision =
    { htmlCode : String
    , elmCode : String
    , dependencies : List Dependency
    , revisionNumber : Int
    , projectId : Uuid
    }


encode : ExistingRevision -> Value
encode revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "dependencies", Encode.list <| List.map Dependency.encode revision.dependencies )
        , ( "revisionNumber", Encode.int revision.revisionNumber )
        , ( "projectId", Uuid.encode revision.projectId )
        ]


decode : Decoder ExistingRevision
decode =
    Decode.decode ExistingRevision
        |> Decode.required "htmlCode" Decode.string
        |> Decode.required "elmCode" Decode.string
        |> Decode.required "dependencies" (Decode.list Dependency.decode)
        |> Decode.required "revisionNumber" Decode.int
        |> Decode.required "projectId" Uuid.decode
