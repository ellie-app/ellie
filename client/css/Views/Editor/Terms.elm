module Views.Editor.Terms exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


popout : UniqueClass
popout =
    uniqueClass
        [ padding (px 12)
        , backgroundColor (hex Colors.white)
        , position absolute
        , width (px 300)
        , zIndex (int 2)
        , left (px 228)
        , top (px 16)
        , borderRadius (px 3)
        , border3 (px 2) solid (hex Colors.lightGray)
        , backgroundColor (hex Colors.mediumGray)
        , color (hex Colors.white)
        , textAlign center
        ]


content : UniqueClass
content =
    uniqueClass
        [ paddingBottom (px 12)
        ]


link : UniqueClass
link =
    uniqueClass
        [ color (hex Colors.white)
        , fontWeight bold
        , textDecoration none
        ]
