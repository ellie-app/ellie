module Views.Modal exposing (..)

import Colors
import Css exposing (..)
import Css.Extra exposing (..)
import Css.File exposing (..)


container : UniqueClass
container =
    uniqueClass
        [ position absolute
        , width (pct 100)
        , height (pct 100)
        , top zero
        , left zero
        , zIndex (int 5)
        , displayFlex
        , flexDirection column
        , alignItems center
        , paddingTop (pct 25)
        ]


backdrop : UniqueClass
backdrop =
    uniqueClass
        [ position absolute
        , width (pct 100)
        , height (pct 100)
        , top zero
        , left zero
        , zIndex (int 1)
        , backdropFilter <| blur (px 10)
        , backgroundColor <|
            rgba
                (.r Colors.lightGrayRgb)
                (.g Colors.lightGrayRgb)
                (.b Colors.lightGrayRgb)
                0.75
        ]


content : UniqueClass
content =
    uniqueClass
        [ width (pct 50)
        , zIndex (int 2)
        , backgroundColor (hex Colors.darkGray)
        , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.5)
        , borderRadius (px 3)
        ]
