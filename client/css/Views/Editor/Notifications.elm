module Views.Editor.Notifications exposing (..)

import Colors as Colors
import Css exposing (..)
import Css.File exposing (..)


notifications : UniqueClass
notifications =
    uniqueClass
        [ position absolute
        , bottom zero
        , right zero
        , width (px 400)
        , zIndex (int 10)
        , padding (px 16)
        , maxHeight (pct 100)
        , overflowY auto
        ]


item : UniqueClass
item =
    uniqueClass
        [ backgroundColor (hex Colors.darkGray)
        , marginBottom (px 8)
        , padding (px 16)
        , position relative
        , color (hex Colors.white)
        , property "align-items" "center"
        , property "box-shadow" "-4px 4px 10px -4px rgba(0, 0, 0, 0.5)"
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
