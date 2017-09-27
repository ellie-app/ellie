module Ellie.Ui.Toast exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


warning =
    uniqueClass [ borderColor Colors.yellow_ ]


error =
    uniqueClass [ borderColor Colors.red_ ]


success =
    uniqueClass [ borderColor Colors.green_ ]


info =
    uniqueClass [ borderColor Colors.blue_ ]


container : UniqueClass
container =
    uniqueClass
        [ backgroundColor Colors.darkGray_
        , marginBottom (px 8)
        , padding (px 16)
        , position relative
        , color (hex Colors.white)
        , property "align-items" "center"
        , Colors.boxShadowPopout
        , borderRadius (px 2)
        , lastChild [ marginBottom zero ]
        , borderLeft3 (px 2) solid (hex Colors.green)
        ]


itemMessage : UniqueClass
itemMessage =
    uniqueClass
        [ property "white-space" "pre-wrap"
        , fontWeight normal
        , fontSize (px 16)
        , overflowX auto
        ]


itemTitle : UniqueClass
itemTitle =
    uniqueClass
        [ fontSize (px 12)
        , textTransform uppercase
        , fontWeight bold
        ]


itemTimestamp : UniqueClass
itemTimestamp =
    uniqueClass
        [ fontSize (px 12)
        , paddingBottom (px 12)
        ]


itemActions : UniqueClass
itemActions =
    uniqueClass
        [ paddingTop (px 12)
        ]


buttonBase : Style
buttonBase =
    batch
        [ fontFamily inherit
        , fontSize (px 12)
        , fontWeight bold
        , textTransform uppercase
        , padding (px 8)
        , borderRadius (px 3)
        , border zero
        , cursor pointer
        , margin2 zero (px 4)
        , firstChild [ marginLeft zero ]
        , lastChild [ marginRight zero ]
        , color (hex Colors.white)
        , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.5)
        ]


itemActionButton : UniqueClass
itemActionButton =
    uniqueClass
        [ backgroundColor (hex Colors.red)
        , buttonBase
        ]


closeButton : UniqueClass
closeButton =
    uniqueClass
        [ backgroundColor (hex Colors.mediumGray)
        , color (hex Colors.white)
        , buttonBase
        ]
