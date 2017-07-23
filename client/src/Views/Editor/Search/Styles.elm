module Views.Editor.Search.Styles exposing (styles)

import Css exposing (..)
import Css.Namespace exposing (..)
import Shared.Colors as Colors
import Views.Editor.Search.Classes exposing (..)


styles : Stylesheet
styles =
    (stylesheet << namespace "components_search_")
        [ Css.class SearchBar
            [ position relative
            , overflow hidden
            ]
        , Css.class SearchBarIcon
            [ width (px 26)
            , height (px 26)
            , color (hex Colors.lightGray)
            , left (px 16)
            , position absolute
            , top (pct 50)
            , marginTop (px -13)
            ]
        , Css.class SearchBarInput
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
        , Css.class Results
            [ backgroundColor (hex Colors.mediumGray)
            , padding (px 16)
            ]
        , Css.class ResultsItem
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
        , Css.class ResultsItemInfo
            [ padding2 zero (px 16)
            , displayFlex
            , alignItems center
            , property "width" "calc(100% - 96px)"
            ]
        , Css.class ResultsItemName
            [ fontSize (px 24)
            , lineHeight (px 30)
            , color (hex Colors.white)
            , paddingRight (px 8)
            , overflowX hidden
            , whiteSpace noWrap
            , textOverflow ellipsis
            ]
        , Css.class ResultsItemVersion
            [ color (hex Colors.lightGray)
            , fontSize (px 20)
            , lineHeight (px 26)
            ]
        , Css.class ResultsItemButtonGroup
            [ displayFlex
            , height (pct 100)
            ]
        , Css.class ResultsItemButton
            [ property "background" "none"
            , border zero
            , height (pct 100)
            , color (hex Colors.lightGray)
            , borderLeft3 (px 2) solid (hex Colors.mediumGray)
            , width (px 48)
            , cursor pointer
            , textDecoration none
            ]
        , Css.class ResultsItemButtonInner
            [ displayFlex
            , flexDirection column
            , alignItems center
            , property "justify-content" "center"
            ]
        , Css.class ResultsItemButtonIcon
            [ width (px 16)
            , height (px 16)
            , display block
            ]
        , Css.class ResultsItemButtonText
            [ fontSize (px 8)
            , textTransform uppercase
            , lineHeight (px 10)
            , paddingTop (px 2)
            ]
        ]
