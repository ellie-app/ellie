module Pages.Editor.Output exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container =
    uniqueClass
        [ position relative
        , width (pct 100)
        , height (pct 100)
        , backgroundColor Colors.darkGray_
        , displayFlex
        , flexDirection column
        , alignItems center
        ]


iframe =
    uniqueClass
        [ width (pct 100)
        , height (pct 100)
        , backgroundColor (hex "fff")
        , border zero
        ]


details =
    uniqueClass
        [ width <| calc (pct 100) minus (px 96)
        , backgroundColor Colors.darkMediumGray
        , borderRadius (px 2)
        , Colors.boxShadowBottom
        , marginTop (px 48)
        , padding2 (px 48) (px 16)
        , color Colors.lightGray_
        , displayFlex
        , flexDirection column
        , alignItems center
        ]


detailsTitle =
    uniqueClass
        [ fontSize (px 24)
        ]


detailsSubMessage =
    uniqueClass
        [ fontSize (px 16)
        , color Colors.lightMediumGray
        ]


progressBarContainer =
    uniqueClass
        [ width (pct 100)
        , maxWidth (px 240)
        , marginTop (px 24)
        ]


detailsExtraNotice =
    uniqueClass
        [ fontSize (px 16)
        , color Colors.lightMediumGray
        , marginTop (px 24)
        , textAlign center
        , maxWidth (px 360)
        ]
