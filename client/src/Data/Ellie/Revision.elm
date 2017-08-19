module Data.Ellie.Revision
    exposing
        ( Revision
        , Snapshot(..)
        , decoder
        , empty
        , encoder
        , moduleName
        , toDescription
        )

import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Data.Elm.Compiler.Error as CompilerError
import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Constraint as Constraint exposing (Constraint)
import Data.Elm.Package.Description as Description exposing (Description)
import Data.Elm.Package.Version as Version exposing (Version)
import Extra.Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)
import Shared.Utils as Utils


moduleName : Revision -> String
moduleName revision =
    Regex.find (Regex.AtMost 1) (Regex.regex "^module ([a-zA-Z.]+) exposing") revision.elmCode
        |> List.head
        |> Maybe.andThen (.submatches >> List.head)
        |> Maybe.andThen identity
        |> Maybe.withDefault "Main"


type Snapshot
    = NotSaved
    | Uploaded
    | Errored (List CompilerError.Error)


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , owned : Bool
    , id : Maybe RevisionId
    , title : String
    , description : String
    , snapshot : Snapshot
    , elmVersion : Version
    , acceptedTerms : Maybe TermsVersion
    }


empty : Revision
empty =
    { htmlCode = ""
    , elmCode = ""
    , packages = []
    , owned = False
    , id = Nothing
    , title = ""
    , description = ""
    , snapshot = NotSaved
    , elmVersion = Version 0 0 0
    , acceptedTerms = Nothing
    }


encoder : Revision -> Value
encoder revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "packages", Encode.list <| List.map Package.encoder revision.packages )
        , ( "owned", Encode.bool revision.owned )
        , ( "id", Utils.encodeNullable RevisionId.encoder revision.id )
        , ( "title", Encode.string revision.title )
        , ( "description", Encode.string revision.description )
        , ( "snapshot", encodeSnapshot revision.snapshot )
        , ( "elmVersion", Version.encoder revision.elmVersion )
        , ( "acceptedTerms", Encode.maybeNull TermsVersion.encoder revision.acceptedTerms )
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
        |> Decode.optional "owned" Decode.bool True
        |> Decode.required "id" (Decode.nullable RevisionId.decoder)
        |> Decode.required "title" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.optional "snapshot" decodeSnapshot NotSaved
        |> Decode.optional "elmVersion" Version.decoder (Version 0 18 0)
        |> Decode.optional "acceptedTerms" (Decode.nullable TermsVersion.decoder) Nothing


toDescription : Revision -> Description
toDescription revision =
    Description
        { user = "user", project = "project" }
        "https://github.com/user/project.git"
        (Version 1 0 0)
        (Constraint.fromVersion <| revision.elmVersion)
        revision.description
        "BSD3"
        [ "." ]
        []
        False
        (List.map (Tuple.mapSecond Constraint.fromVersion) revision.packages)
