module Ellie.Types.Revision
    exposing
        ( Id
        , Revision
        , decoder
        , empty
        , encoder
        , idDecoder
        , idEncoder
        , idEq
        )

import Data.Elm.Compiler.Error as CompilerError
import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version exposing (Version)
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type alias Id =
    { projectId : String
    , revisionNumber : Int
    }


idEq : Id -> Id -> Bool
idEq left right =
    left.projectId
        == right.projectId
        && left.revisionNumber
        == right.revisionNumber


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
    , elmVersion : Version
    , termsVersion : TermsVersion
    }


empty : Revision
empty =
    { htmlCode = ""
    , elmCode = ""
    , packages = []
    , title = ""
    , elmVersion = Version 0 19 0
    , termsVersion = TermsVersion.zero
    }


encoder : Revision -> Value
encoder revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "packages", Encode.list <| List.map Package.encoder revision.packages )
        , ( "title", Encode.string revision.title )
        , ( "elmVersion", Version.encoder revision.elmVersion )
        , ( "termsVersion", TermsVersion.encoder revision.termsVersion )
        ]


decoder : Decoder Revision
decoder =
    Decode.decode Revision
        |> Decode.required "htmlCode" Decode.string
        |> Decode.required "elmCode" Decode.string
        |> Decode.required "packages" (Decode.list Package.decoder)
        |> Decode.required "title" Decode.string
        |> Decode.required "elmVersion" Version.decoder
        |> Decode.required "termsVersion" TermsVersion.decoder
