module Data.Aws.UploadSignature exposing (..)

import Json.Decode as Decode exposing (Decoder)


type alias UploadSignature =
    { url : String
    , fields : List ( String, String )
    , projectId : String
    , revisionNumber : Int
    }


decoder : Decoder UploadSignature
decoder =
    Decode.map4 UploadSignature
        (Decode.field "url" Decode.string)
        (Decode.field "fields" (Decode.keyValuePairs Decode.string))
        (Decode.field "projectId" Decode.string)
        (Decode.field "revisionNumber" Decode.int)
