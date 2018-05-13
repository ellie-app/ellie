module Pages.Editor.Types.Revision exposing (..)

import Data.Uuid as Uuid exposing (Uuid)
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)
import Extra.Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias Revision =
    { htmlCode : String
    , elmCode : String
    , packages : List Package
    , title : String
    , elmVersion : Version
    , userId : Maybe Uuid
    }


localStorageDecoder : Decoder Revision
localStorageDecoder =
    Decode.map6 Revision
        (Decode.field "htmlCode" Decode.string)
        (Decode.field "elmCode" Decode.string)
        (Decode.field "packages" (Decode.list Package.decoder))
        (Decode.field "title" Decode.string)
        (Decode.field "elmVersion" Version.decoder)
        (Decode.field "userId" (Decode.nullable Uuid.decoder))


localStorageEncoder : Revision -> Value
localStorageEncoder revision =
    Encode.object
        [ ( "htmlCode", Encode.string revision.htmlCode )
        , ( "elmCode", Encode.string revision.elmCode )
        , ( "packages", Encode.list <| List.map Package.encoder revision.packages )
        , ( "title", Encode.string revision.title )
        , ( "elmVersion", Version.encoder revision.elmVersion )
        , ( "userId", Encode.maybeNull Uuid.encoder revision.userId )
        ]
