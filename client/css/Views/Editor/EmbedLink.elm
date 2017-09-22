module Views.Editor.EmbedLink exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container : UniqueClass
container =
    uniqueClass
        [ position absolute
        , backgroundColor (hex Colors.mediumGray)
        , width (px 300)
        , boxShadow5 (px -6) (px 6) (px 15) (px -4) (rgba 0 0 0 0.5)
        , border3 (px 1) solid (hex Colors.lightGray)
        , borderRadius (px 3)
        , padding (px 16)
        , color (hex Colors.white)
        , fontSize (px 14)
        , overflowY auto
        , left (px 448)
        , top (px 16)
        , zIndex (int 2)
        ]


link : UniqueClass
link =
    uniqueClass
        [ backgroundColor (hex Colors.darkGray)
        , marginBottom (px 2)
        , padding3 (px 8) (px 8) (px 16)
        , firstChild
            [ borderTopLeftRadius (px 3)
            , borderTopRightRadius (px 3)
            ]
        , lastChild
            [ borderBottomLeftRadius (px 3)
            , borderBottomRightRadius (px 3)
            , marginBottom (px 2)
            ]
        ]


linkTitle : UniqueClass
linkTitle =
    uniqueClass
        [ fontSize (px 12)
        , lineHeight (px 12)
        , textTransform uppercase
        , color (hex Colors.lightGray)
        ]


linkContent : UniqueClass
linkContent =
    uniqueClass
        [ paddingTop (px 8)
        , whiteSpace noWrap
        , overflowX hidden
        , fontFamily monospace
        , property "-webkit-appearance" "none"
        , property "background" "none"
        , border zero
        , display block
        , width (pct 100)
        , fontSize (px 14)
        , color (hex Colors.white)
        ]


buttons : UniqueClass
buttons =
    uniqueClass
        [ paddingTop (px 16)
        ]


button : UniqueClass
button =
    uniqueClass
        [ display block
        , backgroundColor (hex Colors.darkGray)
        , color (hex Colors.lightGray)
        , borderRadius (px 3)
        , border zero
        , width (pct 100)
        , padding (px 12)
        , fontSize (px 16)
        , textTransform uppercase
        , fontFamily inherit
        , fontWeight (int 500)
        , cursor pointer
        , disabled
            [ opacity (num 0.5)
            ]
        ]


buttonInner : UniqueClass
buttonInner =
    uniqueClass
        [ displayFlex
        , property "justify-content" "center"
        , alignItems center
        ]


buttonIcon : UniqueClass
buttonIcon =
    uniqueClass
        [ width (px 20)
        , height (px 20)
        , marginRight (px 12)
        ]
