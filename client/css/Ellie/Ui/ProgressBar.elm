module Ellie.Ui.ProgressBar exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


outer =
    uniqueClass
        [ width (pct 100)
        , height (px 8)
        , overflowX hidden
        , backgroundColor Colors.mediumGray_
        ]


innerIndeterminate =
    uniqueClass
        [ property "animation" "1.5s infinite ProgressBar-indeterminate"
        , property "transform-origin" "left"
        , height (px 8)
        , width (pct 100)
        , backgroundColor Colors.pink_
        ]


innerPercentage =
    uniqueClass
        [ property "transform-origin" "left"
        , height (px 8)
        , property "transition" "transform 150ms"
        , width (pct 100)
        , backgroundColor Colors.pink_
        ]
