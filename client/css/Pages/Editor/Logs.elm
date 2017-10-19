module Pages.Editor.Logs exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container =
    uniqueClass
        [ width (pct 100)
        , displayFlex
        , flexDirection column
        ]


controls =
    uniqueClass
        [ displayFlex
        , width (pct 100)
        , padding (px 12)
        , backgroundColor Colors.darkMediumGray
        , flexShrink (int 0)
        ]


filterInput =
    uniqueClass
        [ width (pct 100)
        , flexShrink (int 1)
        , paddingRight (px 12)
        ]


clearButton =
    uniqueClass
        [ flexShrink (int 0)
        ]


logs =
    uniqueClass
        [ height (pct 100)
        , flexShrink (int 1)
        , displayFlex
        , padding (px 12)
        , overflowY auto
        , flexDirection column
        ]


log =
    uniqueClass
        [ backgroundColor Colors.darkMediumGray
        , Colors.boxShadow |> .bottom
        , padding (px 12)
        , borderRadius (px 2)
        , marginBottom (px 12)
        , lastChild [ marginBottom zero ]
        ]


label =
    uniqueClass
        [ color Colors.lightMediumGray
        , fontWeight bold
        , fontSize (px 14)
        , marginBottom (px 8)
        ]


body =
    uniqueClass
        [ color Colors.lightGray
        , fontSize (px 18)
        , fontFamily monospace
        ]
