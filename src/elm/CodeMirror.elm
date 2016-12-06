module CodeMirror
    exposing
        ( Position
        , LinterMessage
        , Severity(..)
        , position
        , linterMessage
        , mode
        , onUpdate
        , linterMessages
        , editor
        )

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode
import Html exposing (Html, Attribute)
import Html.Attributes exposing (property)
import Html.Events exposing (on, targetValue)
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
    on "CodeMirror.updated" (Decode.map tagger targetValue)


mode : String -> Attribute msg
mode =
    Encode.string >> property "mode"


editor : List (Attribute msg) -> Html msg
editor =
    Native.CodeMirror.editor


linterMessages : List LinterMessage -> Attribute msg
linterMessages messages =
    messages
        |> List.map encodeLinterMessage
        |> Encode.list
        |> property "linterMessages"


encodeLinterMessage : LinterMessage -> Value
encodeLinterMessage =
    Native.CodeMirror.encodeLinterMessage
