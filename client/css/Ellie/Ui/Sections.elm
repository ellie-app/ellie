module Ellie.Ui.Sections exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container =
    uniqueClass
        [ position relative
        , displayFlex
        , flexDirection column
        , height (pct 100)
        ]


section =
    uniqueClass
        [ position relative
        , displayFlex
        , flexDirection column
        , width (pct 100)
        ]


closedSection =
    uniqueClass
        [ flexShrink (int 0) ]


openSection =
    uniqueClass
        [ flexShrink (int 1) ]


content =
    uniqueClass
        [ position relative
        , flexShrink (int 1)
        , overflowY auto
        ]


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
        , flexShrink (int 0)
        ]


buttonOpen =
    uniqueClass
        [ Colors.boxShadow |> .bottom
        , borderBottom3 (px 1) solid Colors.pink_
        , cursor default
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
