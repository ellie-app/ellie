module CodeMirror exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Html exposing (Html, Attribute)
import Html.Attributes exposing (property)
import Html.Events exposing (on, targetValue)
import Native.CodeMirror


onUpdate : (String -> msg) -> Attribute msg
onUpdate tagger =
    on "CodeMirror.updated" (Decode.map tagger targetValue)


mode : String -> Attribute msg
mode =
    Encode.string >> property "mode"


editor : List (Attribute msg) -> Html msg
editor =
    Native.CodeMirror.editor
