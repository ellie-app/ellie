module Views.Editor.Search exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


searchBar : UniqueClass
searchBar =
    uniqueClass
        [ position relative
        , overflow hidden
        ]


searchBarIcon : UniqueClass
searchBarIcon =
    uniqueClass
        [ width (px 26)
        , height (px 26)
        , color (hex Colors.lightGray)
        , left (px 16)
        , position absolute
        , top (pct 50)
        , marginTop (px -13)
        ]


searchBarInput : UniqueClass
searchBarInput =
    uniqueClass
        [ property "background" "none"
        , width (pct 100)
        , display block
        , border zero
        , fontSize (px 20)
        , fontFamily inherit
        , lineHeight (px 26)
        , padding4 (px 12) (px 16) (px 12) (px 58)
        , color (hex Colors.white)
        ]


results : UniqueClass
results =
    uniqueClass
        [ backgroundColor (hex Colors.mediumGray)
        , padding (px 16)
        ]


resultsItem : UniqueClass
resultsItem =
    uniqueClass
        [ backgroundColor (hex Colors.darkGray)
        , marginBottom (px 2)
        , property "justify-content" "space-between"
        , displayFlex
        , height (px 60)
        , firstChild
            [ borderTopRightRadius (px 3)
            , borderTopLeftRadius (px 3)
            ]
        , lastChild
            [ borderBottomLeftRadius (px 3)
            , borderBottomRightRadius (px 3)
            , marginBottom zero
            ]
        ]


resultsItemInfo : UniqueClass
resultsItemInfo =
    uniqueClass
        [ padding2 zero (px 16)
        , displayFlex
        , alignItems center
        , property "width" "calc(100% - 96px)"
        ]


resultsItemName : UniqueClass
resultsItemName =
    uniqueClass
        [ fontSize (px 24)
        , lineHeight (px 30)
        , color (hex Colors.white)
        , paddingRight (px 8)
        , overflowX hidden
        , whiteSpace noWrap
        , textOverflow ellipsis
        ]


resultsItemVersion : UniqueClass
resultsItemVersion =
    uniqueClass
        [ color (hex Colors.lightGray)
        , fontSize (px 20)
        , lineHeight (px 26)
        ]


resultsItemButtonGroup : UniqueClass
resultsItemButtonGroup =
    uniqueClass
        [ displayFlex
        , height (pct 100)
        ]


resultsItemButton : UniqueClass
resultsItemButton =
    uniqueClass
        [ property "background" "none"
        , border zero
        , height (pct 100)
        , color (hex Colors.lightGray)
        , borderLeft3 (px 2) solid (hex Colors.mediumGray)
        , width (px 48)
        , cursor pointer
        , textDecoration none
        ]


resultsItemButtonInner : UniqueClass
resultsItemButtonInner =
    uniqueClass
        [ displayFlex
        , flexDirection column
        , alignItems center
        , property "justify-content" "center"
        ]


resultsItemButtonIcon : UniqueClass
resultsItemButtonIcon =
    uniqueClass
        [ width (px 16)
        , height (px 16)
        , display block
        ]


resultsItemButtonText : UniqueClass
resultsItemButtonText =
    uniqueClass
        [ fontSize (px 8)
        , textTransform uppercase
        , lineHeight (px 10)
        , paddingTop (px 2)
        ]
