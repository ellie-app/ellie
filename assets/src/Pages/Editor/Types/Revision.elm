module Pages.Editor.Types.Revision exposing (..)

import Data.Url as Url exposing (Url)
import Ellie.Constants as Constants
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Id =
    String


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , title : String
    , elmVersion : Version
    }


editorLink : Id -> Url
editorLink id =
    Url.fromString <| Constants.editorBase ++ "/" ++ id


embedLink : Id -> Url
embedLink id =
    Url.fromString <| Constants.embedBase ++ "/" ++ id


localStorageDecoder : Decoder Revision
localStorageDecoder =
    Decode.map5 Revision
        (Decode.field "htmlCode" Decode.string)
        (Decode.field "elmCode" Decode.string)
        (Decode.field "packages" (Decode.list Package.decoder))
        (Decode.field "title" Decode.string)
        (Decode.field "elmVersion" Version.decoder)


localStorageEncoder : Revision -> Value
localStorageEncoder revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "packages", Encode.list <| List.map Package.encoder revision.packages )
        , ( "title", Encode.string revision.title )
        , ( "elmVersion", Version.encoder revision.elmVersion )
        ]
