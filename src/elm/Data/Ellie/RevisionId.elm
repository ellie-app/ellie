module Data.Ellie.RevisionId
    exposing
        ( RevisionId
        , decoder
        , encoder
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias RevisionId =
    { projectId : String
    , revisionNumber : Int
    }


decoder : Decoder RevisionId
decoder =
    Decode.map2 RevisionId
        (Decode.field "projectId" Decode.string)
        (Decode.field "revisionNumber" Decode.int)


encoder : RevisionId -> Value
encoder revisionId =
    Encode.object
        [ ( "projectId", Encode.string revisionId.projectId )
        , ( "revisionNumber", Encode.int revisionId.revisionNumber )
        ]
