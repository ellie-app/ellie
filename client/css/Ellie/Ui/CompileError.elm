module Ellie.Ui.CompileError exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container =
    uniqueClass
        [ width (pct 100)
        , backgroundColor Colors.darkMediumGray
        , Colors.boxShadow |> .bottom
        , padding2 (px 12) (px 16)
        , borderLeft3 (px 1) solid Colors.red_
        , color Colors.lightGray_
        , position relative
        ]


header =
    uniqueClass
        [ displayFlex
        , justifyContent spaceBetween
        , paddingBottom (px 12)
        ]


tag =
    uniqueClass
        [ fontSize (px 16)
        , fontWeight bold
        ]


location =
    uniqueClass
        [ fontSize (px 16)
        , color Colors.lightMediumGray
        ]


overview : UniqueClass
overview =
    uniqueClass
        [ fontSize (px 16)
        , paddingBottom (px 16)
        , lineHeight (px 16)
        , color Colors.lightMediumGray
        ]


details : UniqueClass
details =
    uniqueClass
        [ fontSize (px 16)
        , color Colors.lightMediumGray
        ]
