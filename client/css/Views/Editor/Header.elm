module Views.Editor.Header exposing (..)

import Colors
import Constants
import Css exposing (..)
import Css.File exposing (..)


header : UniqueClass
header =
    uniqueClass
        [ width (pct 100)
        , height (px Constants.headerHeight)
        , backgroundColor (hex Colors.darkGray)
        , displayFlex
        , alignItems center
        , boxShadow5 (px 0) (px 2) (px 15) (px -2) (rgba 0 0 0 0.5)
        , position relative
        , property "z-index" "4"
        , property "justify-content" "space-between"
        , padding2 zero (px 16)
        , border zero
        , property "border-image" <| Colors.pinkPurpleGradient ++ " 1"
        , borderBottomWidth (px 2)
        , borderBottomStyle solid
        ]


headerGroup : UniqueClass
headerGroup =
    uniqueClass
        [ displayFlex
        , alignItems center
        ]


logo : UniqueClass
logo =
    uniqueClass
        [ fontFamilies [ Constants.scriptFont ]
        , fontSize (px 32)
        , color (hex Colors.white)
        , paddingRight (px 24)
        ]


button : UniqueClass
button =
    uniqueClass
        [ backgroundColor <|
            rgba
                (.r Colors.mediumGrayRgb)
                (.g Colors.mediumGrayRgb)
                (.b Colors.mediumGrayRgb)
                0.24
        , border zero
        , borderRadius (px 3)
        , color (hex Colors.white)
        , fontFamily inherit
        , textTransform uppercase
        , height (px 38)
        , fontSize (px 14)
        , padding2 zero (px 12)
        , cursor pointer
        , marginLeft (px 8)
        , disabled
            [ opacity (num 0.35)
            , cursor notAllowed
            ]
        , hover
            [ backgroundColor <|
                rgba
                    (.r Colors.mediumGrayRgb)
                    (.g Colors.mediumGrayRgb)
                    (.b Colors.mediumGrayRgb)
                    0.4
            , disabled
                [ backgroundColor <|
                    rgba
                        (.r Colors.mediumGrayRgb)
                        (.g Colors.mediumGrayRgb)
                        (.b Colors.mediumGrayRgb)
                        0.24
                ]
            ]
        , active
            [ backgroundColor <|
                rgba
                    (.r Colors.mediumGrayRgb)
                    (.g Colors.mediumGrayRgb)
                    (.b Colors.mediumGrayRgb)
                    0.5
            ]
        ]


buttonIcon : UniqueClass
buttonIcon =
    uniqueClass
        [ height (px 16)
        , width (px 16)
        , display block
        ]


buttonText : UniqueClass
buttonText =
    uniqueClass
        [ marginLeft (px 8)
        ]


buttonInner : UniqueClass
buttonInner =
    uniqueClass
        [ displayFlex
        , property "justify-content" "center"
        , alignItems center
        ]
