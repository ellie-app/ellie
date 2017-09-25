module Ellie.Ui.Icon exposing (..)

import Css exposing (..)
import Css.File exposing (..)


icon : UniqueSvgClass
icon =
    uniqueSvgClass
        [ width (pct 100)
        , height (pct 100)
        , fill currentColor
        , display block
        ]
