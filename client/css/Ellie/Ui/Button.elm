module Ellie.Ui.Button exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


button : UniqueClass
button =
    uniqueClass
        [ border zero
        , fontFamily inherit
        , cursor pointer
        , textDecoration none
        , property "user-select" "none"
        , disabled
            [ opacity (num 0.6)
            , cursor notAllowed
            ]
        ]


backgroundShared : Style
backgroundShared =
    batch
        [ borderRadius (px 2)
        , Colors.boxShadowBottom
        , property "transition" "transform 150ms, box-shadow 150ms, background-color 150ms"
        , hover
            [ property "transform" "translateY(-1px)"
            , Colors.boxShadowBottomHover
            ]
        , active
            [ property "transform" "none"
            ]
        , disabled
            [ property "transform" "none"
            , boxShadow none
            , backgroundColor Colors.mediumGray_
            ]
        ]


primary : UniqueClass
primary =
    uniqueClass
        [ backgroundColor Colors.mediumGray_
        , color Colors.lightGray_
        , backgroundShared
        ]


accent : UniqueClass
accent =
    uniqueClass
        [ backgroundColor Colors.pink_
        , color Colors.lightGray_
        , backgroundShared
        ]


link : UniqueClass
link =
    uniqueClass
        [ color Colors.lightMediumGray
        , padding zero |> important
        , property "background" "none"
        , fontWeight bold
        , property "transition" "color 150ms"
        , hover
            [ color Colors.lightGray_
            ]
        , disabled
            [ color Colors.lightMediumGray
            ]
        ]


small : UniqueClass
small =
    uniqueClass
        [ fontSize (px 12)
        , padding2 (px 2) (px 4)
        ]


medium : UniqueClass
medium =
    uniqueClass
        [ fontSize (px 15)
        , padding2 (px 8) (px 12)
        ]


iconMedium : UniqueClass
iconMedium =
    uniqueClass
        [ height (px 16)
        , width (px 16)
        , marginRight (px 6)
        ]


iconSmall : UniqueClass
iconSmall =
    uniqueClass
        [ height (px 12)
        , width (px 12)
        , marginRight (px 2)
        ]


inner : UniqueClass
inner =
    uniqueClass
        [ displayFlex
        , alignItems center
        ]
