module Views.Editor.About exposing (..)

import Colors as Colors
import Css exposing (..)
import Css.File exposing (..)


popout : UniqueClass
popout =
    uniqueClass
        [ position absolute
        , backgroundColor (hex Colors.mediumGray)
        , width (px 400)
        , boxShadow5 (px -6) (px 6) (px 15) (px -4) (rgba 0 0 0 0.5)
        , property "z-index" "4"
        , borderRadius (px 3)
        , padding (px 16)
        , overflowY auto
        , border3 (px 2) solid (hex Colors.lightGray)
        , color (hex Colors.white)
        , right (px 16)
        , top (px 16)
        ]


title : UniqueClass
title =
    uniqueClass
        [ fontSize (px 18)
        , fontWeight (int 700)
        ]


paragraph : UniqueClass
paragraph =
    uniqueClass
        [ fontSize (px 16)
        , lastChild
            [ marginBottom zero ]
        ]


link : UniqueClass
link =
    uniqueClass
        [ color inherit
        , textDecoration none
        , fontWeight (int 700)
        ]
