module Views.Editors.CodeMirror
    exposing
        ( LinterMessage
        , Position
        , Severity(..)
        , editor
        , indentWidth
        , linterMessage
        , linterMessages
        , mode
        , onUpdate
        , position
        , readOnly
        , theme
        , value
        , vimMode
        )

import Html exposing (Attribute, Html)
import Html.Attributes exposing (property)
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Native.CodeMirror


type Position
    = Position


type LinterMessage
    = LinterMessage


type Severity
    = Error
    | Warning


severityToString : Severity -> String
severityToString severity =
    case severity of
        Error ->
            "error"

        Warning ->
            "warning"


nativeLinterMessage : String -> String -> Position -> Position -> LinterMessage
nativeLinterMessage =
    Native.CodeMirror.linterMessage


linterMessage : Severity -> String -> Position -> Position -> LinterMessage
linterMessage severity message from to =
    nativeLinterMessage (severityToString severity) message from to


position : Int -> Int -> Position
position =
    Native.CodeMirror.position


onUpdate : (String -> msg) -> Attribute msg
onUpdate tagger =
    on "CodeMirror.updated" (Decode.map tagger (Decode.at [ "target", "editorValue" ] Decode.string))


mode : String -> Attribute msg
mode =
    Encode.string >> property "mode"


readOnly : Bool -> Attribute msg
readOnly =
    Encode.bool >> property "readOnly"


theme : String -> Attribute msg
theme =
    Encode.string >> property "theme"


editor : List (Attribute msg) -> Html msg
editor =
    Native.CodeMirror.editor


value : String -> Attribute msg
value =
    Encode.string >> property "editorValue"


vimMode : Bool -> Attribute msg
vimMode =
    Encode.bool >> property "vimMode"


indentWidth : Int -> Attribute msg
indentWidth =
    Encode.int >> property "indentWidth"


linterMessages : List LinterMessage -> Attribute msg
linterMessages messages =
    messages
        |> List.map encodeLinterMessage
        |> Encode.list
        |> property "linterMessages"


encodeLinterMessage : LinterMessage -> Value
encodeLinterMessage =
    Native.CodeMirror.encodeLinterMessage
