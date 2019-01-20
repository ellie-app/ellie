module Ellie.Ui.CodeEditor exposing
    ( Attribute
    , Completions
    , LinterMessage
    , Located(..)
    , Position
    , Severity(..)
    , Token(..)
    , autocomplete
    , completions
    , id
    , linterMessages
    , mode
    , noCompletions
    , nowhere
    , onAdvancedToken
    , onChange
    , onSettled
    , onToken
    , readOnly
    , tabSize
    , value
    , view
    , vim
    )

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (property)
import Html.Styled.Events as Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Attribute msg
    = Attr (Html.Attribute msg)


unattr : Attribute msg -> Html.Attribute msg
unattr (Attr a) =
    a


id : String -> Attribute msg
id =
    Attributes.id >> Attr


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


autocomplete : Completions -> Attribute msg
autocomplete (Completions value) =
    Attr <| property "autocomplete" value


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    Attr <| on "change" (Decode.map tagger (Decode.at [ "target", "editorValue" ] Decode.string))


onToken : (Maybe String -> msg) -> Attribute msg
onToken tagger =
    Decode.string
        |> Decode.nullable
        |> Decode.at [ "target", "token" ]
        |> Decode.map tagger
        |> on "settled"
        |> Attr


onAdvancedToken : (Located Token -> msg) -> Attribute msg
onAdvancedToken tagger =
    Decode.at [ "target", "advancedToken" ] (locatedDecoder tokenDecoder)
        |> Decode.map tagger
        |> on "settled"
        |> Attr


onSettled : msg -> Attribute msg
onSettled msg =
    Attr <| on "settled" (Decode.succeed msg)


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


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.field "line" Decode.int)
        (Decode.field "ch" Decode.int)


type Completions
    = Completions Value


completions : Located (List String) -> Completions
completions (Located from to list) =
    case list of
        [] ->
            noCompletions

        stuff ->
            Completions <|
                Encode.object
                    [ ( "from", positionEncoder from )
                    , ( "to", positionEncoder to )
                    , ( "list", Encode.list <| List.map Encode.string stuff )
                    ]


noCompletions : Completions
noCompletions =
    Completions Encode.null


type Located a
    = Located Position Position a


nowhere : a -> Located a
nowhere a =
    Located { line = 0, column = 0 } { line = 0, column = 0 } a


locatedDecoder : Decoder a -> Decoder (Located a)
locatedDecoder decoder =
    Decode.map3 Located
        (Decode.field "from" positionDecoder)
        (Decode.field "to" positionDecoder)
        decoder


type Token
    = Operator String
    | UppercaseVar String (Maybe String)
    | LowercaseVar String (Maybe String)
    | Qualifier String
    | Unknown


tokenDecoder : Decoder Token
tokenDecoder =
    Decode.andThen
        (\tipe ->
            case tipe of
                "Operator" ->
                    Decode.map Operator
                        (Decode.field "text" Decode.string)

                "UppercaseVar" ->
                    Decode.map2 UppercaseVar
                        (Decode.field "text" Decode.string)
                        (Decode.field "qualifier" (Decode.nullable Decode.string))

                "LowercaseVar" ->
                    Decode.map2 LowercaseVar
                        (Decode.field "text" Decode.string)
                        (Decode.field "qualifier" (Decode.nullable Decode.string))

                "Qualifier" ->
                    Decode.map Qualifier
                        (Decode.field "text" Decode.string)

                _ ->
                    Decode.succeed Unknown
        )
        (Decode.field "type" Decode.string)
