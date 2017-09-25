module Ellie.Ui.Setting exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


title =
    uniqueClass
        [ fontSize (px 16)
        , color Colors.lightGray_
        ]


label =
    uniqueClass
        [ display block ]


description =
    uniqueClass
        [ fontSize (px 12)
        , lineHeight (px 15)
        , color Colors.lightMediumGray
        , paddingTop (px 2)
        ]


control =
    uniqueClass
        [ paddingTop (px 12) ]
