module Ellie.Ui.Dropdown exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container : UniqueClass
container =
    uniqueClass
        [ position relative
        , borderBottom3 (px 1) solid Colors.mediumGray
        ]


select : UniqueClass
select =
    uniqueClass
        [ property "background" "none"
        , border zero
        , borderRadius zero
        , property "-webkit-appearance" "none"
        , width (pct 100)
        , fontFamily inherit
        , fontSize (px 15)
        , color Colors.lightGray
        , padding4 (px 8) (px 20) (px 8) zero
        , outline zero
        , cursor pointer
        , property "user-select" "none"
        , focus
            [ generalSiblings
                [ selector "[data-underline]"
                    [ transform <| scaleX 1
                    ]
                , selector "[data-arrow]"
                    [ color Colors.pink
                    ]
                ]
            ]
        , active
            [ generalSiblings
                [ selector "[data-arrow]"
                    [ transforms [ rotate (deg 180), translateY (pct 50) ]
                    ]
                ]
            ]
        , disabled
            [ color Colors.lightMediumGray
            , cursor notAllowed
            , generalSiblings
                [ selector "[data-arrow]"
                    [ color Colors.mediumGray ]
                ]
            ]
        ]


underline : UniqueClass
underline =
    uniqueClass
        [ position absolute
        , bottom (px -1)
        , height (px 1)
        , width (pct 100)
        , backgroundColor Colors.pink
        , transform <| scaleX 0
        , property "transition" "transform 250ms"
        ]


arrow : UniqueClass
arrow =
    uniqueClass
        [ position absolute
        , color Colors.lightMediumGray
        , width (px 12)
        , height (px 12)
        , right zero
        , top (pct 50)
        , transform <| translateY (pct -50)
        , property "pointer-events" "none"
        ]
