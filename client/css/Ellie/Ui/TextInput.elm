module Ellie.Ui.TextInput exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


icon : UniqueClass
icon =
    uniqueClass
        [ width (px 15)
        , height (px 15)
        , color Colors.lightMediumGray
        , flexShrink (int 0)
        , marginRight (px 8)
        , marginTop (px 2)
        ]


container : UniqueClass
container =
    uniqueClass
        [ position relative
        , borderBottom3 (px 1) solid Colors.lightMediumGray
        , displayFlex
        , alignItems flexStart
        ]


input : UniqueClass
input =
    uniqueClass
        [ property "background" "none"
        , border zero
        , display block
        , width (pct 100)
        , fontSize (px 15)
        , lineHeight (num 1)
        , color Colors.lightGray_
        , fontFamily monospace
        , padding3 zero zero (px 8)
        , focus
            [ adjacentSiblings
                [ selector "[data-underline]"
                    [ transform <| scaleX 1 ]
                ]
            ]
        , pseudoElement "-ms-input-placeholder" [ color Colors.mediumGray_ ]
        , pseudoElement "-webkit-input-placeholder" [ color Colors.mediumGray_ ]
        , pseudoElement "-moz-placeholder" [ color Colors.mediumGray_ ]
        ]


underline : UniqueClass
underline =
    uniqueClass
        [ backgroundColor Colors.pink_
        , height (px 1)
        , position absolute
        , bottom (px -1)
        , width (pct 100)
        , transform <| scaleX 0
        , property "transition" "transform 250ms"
        ]


clearButton : UniqueClass
clearButton =
    uniqueClass
        [ property "background" "none"
        , border zero
        , color Colors.mediumGray_
        , width (px 12)
        , height (px 12)
        , right zero
        , top (px 11)
        , padding zero
        , cursor pointer
        , property "transition" "color 250ms"
        , hover
            [ color Colors.lightMediumGray ]
        , marginLeft (px 8)
        , flexShrink (int 0)
        , marginTop (px 2)
        ]
