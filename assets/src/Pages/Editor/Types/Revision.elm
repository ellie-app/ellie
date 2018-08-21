module Pages.Editor.Types.Revision exposing (..)

import Data.Url as Url exposing (Url)
import Ellie.Constants as Constants
import Elm.Compiler as Compiler
import Elm.Package as Package exposing (Package)
import Elm.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type External
    = Default
    | Remote ( Id, Revision )
    | Example Revision


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


default : List Package -> Revision
default packages =
    { packages = packages
    , title = ""
    , elmVersion = Compiler.version
    , htmlCode = """<html>
<head>
  <style>
    /* you can style your program here */
  </style>
</head>
<body>
  <main></main>
  <script>
    var app = Elm.Main.init({ node: document.querySelector('main') })
    // you can use ports and stuff here
  </script>
</body>
</html>
"""
    , elmCode = """module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
"""
    }
