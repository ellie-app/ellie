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
        , Colors.boxShadow |> .bottom
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


failureMessage =
    uniqueClass
        [ backgroundColor Colors.mediumGray_
        , borderLeft3 (px 1) solid Colors.pink_
        , padding (px 12)
        , marginTop (px 16)
        , fontSize (px 16)
        , whiteSpace preWrap
        ]


failureHint =
    uniqueClass
        [ backgroundColor Colors.darkMediumGray
        , borderRadius (px 2)
        , Colors.boxShadow |> .bottom
        , marginTop (px 24)
        , padding (px 16)
        , width <| calc (pct 100) minus (px 96)
        , color Colors.lightGray_
        , displayFlex
        , flexDirection column
        , alignItems center
        ]


hintText =
    uniqueClass
        [ paddingBottom (px 12)
        , maxWidth (px 500)
        , textAlign center
        ]


errorsContainer =
    uniqueClass
        [ padding3 (px 48) (px 48) (px 16)
        , position relative
        , width (pct 100)
        ]


error =
    uniqueClass
        [ marginBottom (px 24)
        , lastChild [ marginBottom zero ]
        ]
