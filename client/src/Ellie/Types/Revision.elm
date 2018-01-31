module Ellie.Types.Revision
    exposing
        ( Id
        , Revision
        , Snapshot(..)
        , decoder
        , empty
        , encoder
        , idDecoder
        , idEncoder
        )

import Data.Elm.Compiler.Error as CompilerError
import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type Snapshot
    = NotSaved
    | Uploaded
    | Errored (List CompilerError.Error)


type alias Id =
    { projectId : String
    , revisionNumber : Int
    }


idEncoder : Id -> Value
idEncoder id =
    Encode.object
        [ ( "projectId", Encode.string id.projectId )
        , ( "revisionNumber", Encode.int id.revisionNumber )
        ]


idDecoder : Decoder Id
idDecoder =
    Decode.decode Id
        |> Decode.required "projectId" Decode.string
        |> Decode.required "revisionNumber" Decode.int


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , title : String
    , description : String
    , snapshot : Snapshot
    , elmVersion : Version
    }


empty : Revision
empty =
    { htmlCode = ""
    , elmCode = ""
    , packages = []
    , title = ""
    , description = ""
    , snapshot = NotSaved
    , elmVersion = Version 0 0 0
    }


encoder : Revision -> Value
encoder revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "packages", Encode.list <| List.map Package.encoder revision.packages )
        , ( "title", Encode.string revision.title )
        , ( "description", Encode.string revision.description )
        , ( "snapshot", encodeSnapshot revision.snapshot )
        , ( "elmVersion", Version.encoder revision.elmVersion )
        ]


encodeSnapshot : Snapshot -> Value
encodeSnapshot snapshot =
    case snapshot of
        Errored errors ->
            Encode.object
                [ ( "result", Encode.string "ERROR" )
                , ( "errors", Encode.list <| List.map CompilerError.encoder errors )
                ]

        Uploaded ->
            Encode.object
                [ ( "result", Encode.string "SUCCESS" )
                ]

        NotSaved ->
            Encode.object [ ( "result", Encode.string "UNKNOWN" ) ]


decodeSnapshot : Decoder Snapshot
decodeSnapshot =
    Decode.field "result" Decode.string
        |> Decode.andThen
            (\snapshotType ->
                case snapshotType of
                    "ERROR" ->
                        Decode.list CompilerError.decoder
                            |> Decode.field "errors"
                            |> Decode.map Errored

                    "SUCCESS" ->
                        Decode.succeed Uploaded

                    _ ->
                        Decode.succeed NotSaved
            )


decoder : Decoder Revision
decoder =
    Decode.decode Revision
        |> Decode.required "htmlCode" Decode.string
        |> Decode.required "elmCode" Decode.string
        |> Decode.required "packages" (Decode.list Package.decoder)
        |> Decode.required "title" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.optional "snapshot" decodeSnapshot NotSaved
        |> Decode.optional "elmVersion" Version.decoder (Version 0 18 0)
