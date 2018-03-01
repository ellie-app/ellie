module Pages.Editor.Views.Output exposing (Attribute, debug, onLog, src, view)

import Css exposing (..)
import Css.Foreign
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Editor.Types.Log as Log exposing (Log)


type Attribute msg
    = Attr (Html.Attribute msg)


unAttr : Attribute msg -> Html.Attribute msg
unAttr (Attr a) =
    a


src : String -> Attribute msg
src value =
    Attr <| Attributes.src value


debug : Bool -> Attribute msg
debug value =
    Attr <| Attributes.property "debug" <| Encode.bool value


onLog : (Log -> msg) -> Attribute msg
onLog callback =
    Attr <| Events.on "log" <| Decode.map callback <| Decode.field "detail" Log.decoder


view : List (Attribute msg) -> Html msg
view attrs =
    Html.node "ellie-pages-editor-views-output"
        (styles :: List.map unAttr attrs)
        []


styles : Html.Attribute msg
styles =
    css
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , backgroundColor (hex "#fff")
        , Css.Foreign.children
            [ Css.Foreign.selector "iframe"
                [ width (pct 100)
                , height (pct 100)
                , border zero
                , outline zero
                ]
            ]
        ]
