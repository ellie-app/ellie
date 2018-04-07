module Ellie.Ui.Menu exposing (..)

import Css exposing (..)
import Ellie.Ui.Icon as Icon exposing (Icon)
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events


type alias Item msg =
    { label : String
    , onSelect : msg
    }


type alias Config msg =
    { icon : Icon
    , items : List (Item msg)
    }


view : Config msg -> Html msg
view config =
    Html.node "ellie-ui-menu"
        [ css
            [ display block
            , width (pct 100)
            , height (pct 100)
            , position relative
            ]
        ]
        [ Html.button
            [ css
                [ display block
                , width (pct 100)
                , height (pct 100)
                , color Theme.secondaryForeground
                , property "background" "none"
                , border zero
                , outline zero
                , hover [ color Theme.primaryForeground ]
                , active [ transform (scale 1.2) ]
                , cursor pointer
                , padding (pct 25)
                ]
            , Attributes.attribute "data-ellie-ui-menu-toggle" ""
            ]
            [ Icon.view config.icon ]
        , Html.node "ellie-ui-menu-items"
            [ css
                [ position absolute
                , top (pct 100)
                , right zero
                , borderRadius (px 5)
                , backgroundColor (hex "#F6F6F6")
                , padding2 (px 4.5) zero
                , zIndex (int 1)
                , display none
                , overflow hidden
                , property "box-shadow" "0 0 1px 0 rgba(0,0,0,0.6), 0 6px 10px 0 rgba(0,0,0,0.2)"
                ]
            ]
            (List.map viewItem config.items)
        ]


viewItem : Item msg -> Html msg
viewItem item =
    Html.button
        [ Events.onClick item.onSelect
        , css
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
            , whiteSpace noWrap
            , hover
                [ backgroundColor (hex "#0464CF")
                , color (hex "#fff")
                ]
            ]
        ]
        [ Html.text item.label ]
