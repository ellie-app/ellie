module Ellie.Ui.TextArea exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container : UniqueClass
container =
    uniqueClass
        [ position relative
        , overflow hidden
        ]


textarea : UniqueClass
textarea =
    uniqueClass
        [ property "background" "none"
        , border zero
        , display block
        , width (pct 100)
        , fontSize (px 15)
        , lineHeight (num 1)
        , color Colors.lightGray_
        , fontFamily monospace
        , padding zero
        , paddingBottom (px 9)
        , marginBottom (px -8)
        , resize vertical
        , minHeight (px 56)
        , outline zero
        , position relative
        , zIndex (int 1)
        , focus
            [ generalSiblings
                [ selector "[data-underline]"
                    [ transform <| scaleX 1 ]
                , selector "[data-resize]"
                    [ fill Colors.pink_
                    , property "transition-delay" "200ms"
                    ]
                ]
            ]
        , pseudoElement "-ms-input-placeholder" [ color Colors.mediumGray_ ]
        , pseudoElement "-webkit-input-placeholder" [ color Colors.mediumGray_ ]
        , pseudoElement "-moz-placeholder" [ color Colors.mediumGray_ ]
        ]


underlineBase : Style
underlineBase =
    batch
        [ height (px 1)
        , position absolute
        , bottom zero
        , width (pct 100)
        ]


underlineBottom : UniqueClass
underlineBottom =
    uniqueClass
        [ underlineBase
        , backgroundColor Colors.lightMediumGray
        ]


underlineTop : UniqueClass
underlineTop =
    uniqueClass
        [ underlineBase
        , transform <| scaleX 0
        , property "transition" "transform 250ms"
        , backgroundColor Colors.pink_
        ]


resizer : UniqueSvgClass
resizer =
    uniqueSvgClass
        [ width (px 7)
        , height (px 7)
        , position absolute
        , bottom zero
        , right zero
        , zIndex (int 0)
        , fill Colors.lightMediumGray
        , property "transition" "fill 50ms"
        , cursor grab
        ]
