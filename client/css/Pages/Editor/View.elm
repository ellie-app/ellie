module Pages.Editor.View exposing (..)

import Colors as Colors
import Constants as Constants
import Css exposing (..)
import Css.Elements exposing (..)
import Css.Extra exposing (..)
import Css.File exposing (..)


htmlStyles : Snippet
htmlStyles =
    html
        [ height (pct 100)
        , backgroundColor Colors.darkGray
        ]


bodyStyles : Snippet
bodyStyles =
    body
        [ height (pct 100)
        , margin zero
        , fontFamilies [ Constants.sansFont ]
        , property "-webkit-font-smoothing" "antialiased"
        ]


everythingStyles : Snippet
everythingStyles =
    everything
        [ boxSizing borderBox ]


buttonStyles : Snippet
buttonStyles =
    button
        [ focus [ outline zero ] ]


inputStyles : Snippet
inputStyles =
    input [ focus [ outline zero ] ]


elmEditor : Snippet
elmEditor =
    id "elmEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 0)
        ]


htmlEditor : Snippet
htmlEditor =
    id "htmlEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        ]


notifications : UniqueClass
notifications =
    uniqueClass
        [ padding (px 16)
        ]
