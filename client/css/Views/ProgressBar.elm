module Views.ProgressBar exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container : UniqueClass
container =
    uniqueClass
        [ width (pct 100)
        , padding2 (px 8) zero
        , position relative
        ]


barContainer : UniqueClass
barContainer =
    uniqueClass
        [ displayFlex
        , position relative
        , property "align-items" "center"
        , property "justify-content" "center"
        , width (pct 100)
        ]


label : UniqueClass
label =
    uniqueClass
        [ paddingBottom (px 12)
        ]


barOuter : UniqueClass
barOuter =
    uniqueClass
        [ maxWidth (px 400)
        , width (pct 80)
        , position relative
        , border3 (px 1) solid (hex Colors.white)
        , height (px 32)
        , borderRadius (px 5)
        , overflow hidden
        ]


barInner : UniqueClass
barInner =
    uniqueClass
        [ position relative
        , height (pct 100)
        , backgroundColor (hex Colors.white)
        , property "transition" "width 0.2s linear"
        ]


count : UniqueClass
count =
    uniqueClass
        [ marginLeft (px 16)
        , whiteSpace noWrap
        , width (px 32)
        , fontSize (px 14)
        , display inlineBlock
        ]
