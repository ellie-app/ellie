port module Ellie.Ui.Output exposing (Attribute, debug, elmSource, html, onCanDebug, onLog, onRuntimeException, reload, view)

import Css exposing (..)
import Css.Foreign
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Editor.Types.Log as Log exposing (Log)


port ellieUiOutputOut : Encode.Value -> Cmd msg


reload : Cmd msg
reload =
    Encode.object [ ( "tag", Encode.string "Reload" ) ]
        |> ellieUiOutputOut


type Attribute msg
    = Attr (Html.Attribute msg)


unAttr : Attribute msg -> Html.Attribute msg
unAttr (Attr a) =
    a


elmSource : String -> Attribute msg
elmSource value =
    Attr <| Attributes.property "elmSource" <| Encode.string value


html : String -> Attribute msg
html value =
    Attr <| Attributes.property "html" <| Encode.string value


debug : Bool -> Attribute msg
debug value =
    Attr <| Attributes.property "debug" <| Encode.bool value


onLog : (Log -> msg) -> Attribute msg
onLog callback =
    Attr <| Events.on "log" <| Decode.map callback <| Decode.field "detail" Log.decoder


onCanDebug : (Bool -> msg) -> Attribute msg
onCanDebug callback =
    Attr <| Events.on "canDebug" <| Decode.map callback <| Decode.field "detail" Decode.bool


view : List (Attribute msg) -> Html msg
view attrs =
    Html.node "ellie-ui-output"
        (styles :: List.map unAttr attrs)
        []


onRuntimeException : (String -> msg) -> Attribute msg
onRuntimeException callback =
    Attr <| Events.on "runtimeException" <| Decode.map callback <| Decode.field "detail" Decode.string


styles : Html.Attribute msg
styles =
    css
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , backgroundColor (hex "#fff")
        , display block
        , Css.Foreign.children
            [ Css.Foreign.selector "iframe"
                [ width (pct 100)
                , height (pct 100)
                , border zero
                , outline zero
                ]
            ]
        ]
