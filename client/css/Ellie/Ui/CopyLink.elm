module Ellie.Ui.CopyLink exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container =
    uniqueClass
        [ backgroundColor Colors.darkMediumGray
        , padding2 (px 4) (px 8)
        ]


button =
    uniqueClass
        [ width (px 28)
        , height (px 28)
        , marginRight (px 4)
        , property "background" "none"
        , border zero
        , cursor pointer
        , color Colors.mediumGray_
        , hover [ color Colors.lightMediumGray ]
        , active [ color Colors.lightGray_ ]
        ]


input =
    uniqueClass
        [ border zero
        , property "background" "none"
        , fontFamily monospace
        , fontSize (px 15)
        , color Colors.lightGray_
        , width (px 240)
        ]


controls =
    uniqueClass [ displayFlex ]


title =
    uniqueClass
        [ display block
        , fontSize (px 12)
        , color Colors.lightMediumGray
        ]
