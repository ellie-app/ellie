module Ellie.Ui.Package exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container : UniqueClass
container =
    uniqueClass
        [ position relative
        , width (pct 100)
        , displayFlex
        , backgroundColor Colors.darkMediumGray
        , Colors.boxShadowBottom
        , borderRadius (px 2)
        , padding (px 8)
        ]


details : UniqueClass
details =
    uniqueClass
        [ width (pct 100)
        , displayFlex
        , flexDirection column
        , justifyContent spaceBetween
        , height (pct 100)
        , paddingRight (px 4)
        , overflowX hidden
        ]


detailsLine : UniqueClass
detailsLine =
    uniqueClass
        [ marginTop (px 6)
        , displayFlex
        , alignItems center
        ]


ellipsisText : Style
ellipsisText =
    batch
        [ whiteSpace noWrap
        , overflowX hidden
        , textOverflow ellipsis
        ]


project : UniqueClass
project =
    uniqueClass
        [ fontSize (px 16)
        , fontWeight bold
        , color Colors.lightGray_
        , ellipsisText
        ]


infoIcon : UniqueClass
infoIcon =
    uniqueClass
        [ width (px 14)
        , height (px 14)
        , marginRight (px 4)
        , color Colors.lightMediumGray
        ]


infoText : UniqueClass
infoText =
    uniqueClass
        [ color Colors.lightMediumGray
        , fontSize (px 14)
        , ellipsisText
        ]


buttons : UniqueClass
buttons =
    uniqueClass
        [ displayFlex
        , flexDirection column
        , justifyContent spaceBetween
        , flexShrink (int 0)
        ]
