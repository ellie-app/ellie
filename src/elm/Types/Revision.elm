module Types.Revision
    exposing
        ( Revision
        , Snapshot(..)
        , encode
        , decode
        , empty
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Types.Dependency as Dependency exposing (Dependency)
import Types.CompileError as CompileError exposing (CompileError)
import Shared.Utils as Utils


type Snapshot
    = NotSaved
    | Uploaded
    | Errored (List CompileError)


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , dependencies : List Dependency
    , owned : Bool
    , revisionNumber : Maybe Int
    , projectId : Maybe String
    , title : String
    , description : String
    , snapshot : Snapshot
    }


empty : Revision
empty =
    { htmlCode = ""
    , elmCode = ""
    , dependencies = []
    , owned = False
    , revisionNumber = Nothing
    , projectId = Nothing
    , title = ""
    , description = ""
    , snapshot = NotSaved
    }


encode : Revision -> Value
encode revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "dependencies", Encode.list <| List.map Dependency.encode revision.dependencies )
        , ( "owned", Encode.bool revision.owned )
        , ( "revisionNumber", Utils.encodeNullable Encode.int revision.revisionNumber )
        , ( "projectId", Utils.encodeNullable Encode.string revision.projectId )
        , ( "title", Encode.string revision.title )
        , ( "description", Encode.string revision.description )
        ]


decodeSnapshot : Decoder Snapshot
decodeSnapshot =
    Decode.field "result" Decode.string
        |> Decode.andThen
            (\snapshotType ->
                case snapshotType of
                    "ERROR" ->
                        Decode.list CompileError.decode
                            |> Decode.field "errors"
                            |> Decode.map Errored

                    "SUCCESS" ->
                        Decode.succeed Uploaded

                    _ ->
                        Decode.succeed NotSaved
            )


decode : Decoder Revision
decode =
    Decode.decode Revision
        |> Decode.required "htmlCode" Decode.string
        |> Decode.required "elmCode" Decode.string
        |> Decode.required "dependencies" (Decode.list Dependency.decode)
        |> Decode.optional "owned" Decode.bool True
        |> Decode.required "revisionNumber" (Decode.nullable Decode.int)
        |> Decode.required "projectId" (Decode.nullable Decode.string)
        |> Decode.required "title" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.optional "snapshot" decodeSnapshot NotSaved
