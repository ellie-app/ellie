module Ellie.Ui.Sections exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container =
    uniqueClass
        []


button =
    uniqueClass
        [ display block
        , property "background" "none"
        , border zero
        , outline zero
        , backgroundColor Colors.darkGray_
        , color Colors.lightGray_
        , height (px 44)
        , padding2 zero (px 12)
        , width (pct 100)
        , fontFamily inherit
        , fontWeight bold
        , fontSize (px 16)
        , cursor pointer
        ]


buttonOpen =
    uniqueClass
        [ Colors.boxShadowBottom
        , borderBottom3 (px 1) solid Colors.pink_
        ]


buttonInner =
    uniqueClass
        [ displayFlex
        , alignItems center
        ]


arrow =
    uniqueClass
        [ width (px 14)
        , height (px 14)
        , flexShrink (int 0)
        , marginRight (px 12)
        , transform <| rotate (deg 270)
        ]


arrowOpen =
    uniqueClass
        [ color Colors.pink_
        , transform none
        ]


icon =
    uniqueClass
        [ width (px 16)
        , height (px 16)
        , flexShrink (int 0)
        , marginRight (px 12)
        ]
