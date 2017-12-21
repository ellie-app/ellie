module Ellie.Ui.Toast exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


warning =
    uniqueClass [ borderColor Colors.yellow ]


error =
    uniqueClass [ borderColor Colors.red ]


success =
    uniqueClass [ borderColor Colors.green ]


info =
    uniqueClass [ borderColor Colors.blue ]


container : UniqueClass
container =
    uniqueClass
        [ backgroundColor Colors.darkGray
        , marginBottom (px 8)
        , padding (px 16)
        , position relative
        , color Colors.lightGray
        , property "align-items" "center"
        , Colors.boxShadow |> .popout
        , borderRadius (px 2)
        , lastChild [ marginBottom zero ]
        , borderLeft3 (px 2) solid Colors.green
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


closeButton : UniqueClass
closeButton =
    uniqueClass
        [ color Colors.mediumGray
        , property "background" "none"
        , position absolute
        , border zero
        , top (px 16)
        , right (px 16)
        , width (px 12)
        , height (px 12)
        , padding zero
        , cursor pointer
        , hover [ color Colors.lightMediumGray ]
        , active [ color Colors.lightGray ]
        ]
