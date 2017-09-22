module Views.Button exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


button : UniqueClass
button =
    uniqueClass
        [ display inlineBlock
        , backgroundColor (hex Colors.darkGray)
        , color (hex Colors.lightGray)
        , borderRadius (px 3)
        , border zero
        , padding (px 12)
        , fontSize (px 16)
        , textTransform uppercase
        , fontFamily inherit
        , fontWeight (int 500)
        , cursor pointer
        ]
