module Ellie.Ui.Settings exposing (..)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html, a, button, div, node, text)
import Html.Styled.Attributes as Attributes exposing (css, href)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode


type Action msg
    = Button String msg
    | Link String String


button : String -> msg -> Action msg
button =
    Button


link : String -> String -> Action msg
link =
    Link


viewAction : Action msg -> Html msg
viewAction action =
    case action of
        Button label onClick ->
            Html.button
                [ css
                    [ color (hex "#000")
                    , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif" ]
                    , outline zero
                    , property "background" "none"
                    , border zero
                    , fontSize (px 14)
                    , lineHeight (px 16)
                    , padding4 (px 1.5) (px 34) (px 1.5) (px 20)
                    , display block
                    , cursor pointer
                    , property "user-select" "none"
                    , width (pct 100)
                    , textAlign left
                    , hover
                        [ backgroundColor (hex "#727378")
                        , color (hex "#fff")
                        ]
                    ]
                , Events.onClick onClick
                ]
                [ text label ]

        Link label url ->
            a
                [ css
                    [ color (hex "#000")
                    , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif" ]
                    , outline zero
                    , property "background" "none"
                    , border zero
                    , fontSize (px 14)
                    , lineHeight (px 16)
                    , padding4 (px 1.5) (px 34) (px 1.5) (px 20)
                    , display block
                    , cursor pointer
                    , property "user-select" "none"
                    , whiteSpace noWrap
                    , textDecoration none
                    , width (pct 100)
                    , hover
                        [ backgroundColor (hex "#727378")
                        , color (hex "#fff")
                        ]
                    ]
                , Attributes.href url
                , Attributes.target "_blank"
                ]
                [ text label ]


view : List (Action msg) -> Html msg
view actions =
    node "ellie-ui-settings-container"
        [ css
            [ position relative
            , display inlineBlock
            ]
        ]
        [ node "ellie-ui-settings-handle"
            [ css
                [ width (px 16)
                , height (px 16)
                , color Theme.primaryForeground
                , display block
                , cursor pointer
                , property "user-select" "none"
                ]
            ]
            [ Icon.view Icon.Settings
            ]
        , node "ellie-ui-settings-actions"
            [ css
                [ borderRadius (px 5)
                , backgroundColor (hex "#f6f6f6")
                , position absolute
                , right zero
                , top (pct 100)
                , marginTop (px 8)
                , padding2 (px 4.5) zero
                , overflow hidden
                , display block
                ]
            ]
            (List.map viewAction actions)
        ]
