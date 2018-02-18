module Ellie.Ui.CodeEditor
    exposing
        ( Attribute
        , LinterMessage
        , Position
        , Severity(..)
        , linterMessages
        , mode
        , onChange
        , readOnly
        , tabSize
        , value
        , view
        , vim
        )

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (property)
import Html.Styled.Events as Events exposing (on)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


type Attribute msg
    = Attr (Html.Attribute msg)


unattr : Attribute msg -> Html.Attribute msg
unattr (Attr a) =
    a


value : String -> Attribute msg
value =
    Encode.string >> property "editorValue" >> Attr


mode : String -> Attribute msg
mode =
    Encode.string >> property "mode" >> Attr


vim : Bool -> Attribute msg
vim value =
    Attr <| property "vimMode" <| Encode.bool value


tabSize : Int -> Attribute msg
tabSize =
    Encode.int >> property "tabSize" >> Attr


readOnly : Attribute msg
readOnly =
    Attr <| property "readOnly" <| Encode.bool True


linterMessages : List LinterMessage -> Attribute msg
linterMessages messages =
    Attr <| property "linterMessages" <| Encode.list <| List.map linterMessageEncoder messages


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    Attr <| on "change" (Decode.map tagger (Decode.at [ "target", "editorValue" ] Decode.string))


view : List (Attribute msg) -> Html msg
view attributes =
    Html.node "code-editor" (List.map unattr attributes) []


type Severity
    = Error
    | Warning


type alias Position =
    { line : Int
    , column : Int
    }


type alias LinterMessage =
    { severity : Severity
    , message : String
    , from : Position
    , to : Position
    }


linterMessageEncoder : LinterMessage -> Value
linterMessageEncoder linterMessage =
    Encode.object
        [ ( "severity", severityEncoder linterMessage.severity )
        , ( "message", Encode.string linterMessage.message )
        , ( "from", positionEncoder linterMessage.from )
        , ( "to", positionEncoder linterMessage.to )
        ]


severityEncoder : Severity -> Value
severityEncoder severity =
    case severity of
        Error ->
            Encode.string "error"

        Warning ->
            Encode.string "warning"


positionEncoder : Position -> Value
positionEncoder position =
    Encode.object
        [ ( "line", Encode.int position.line )
        , ( "ch", Encode.int position.column )
        , ( "sticky", Encode.null )
        ]
