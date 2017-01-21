module Components.Search.Styles exposing (styles)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.Search.Classes exposing (..)
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace "components_search_")
        [ (.) Container
            [ position absolute
            , width (pct 100)
            , height (pct 100)
            , top zero
            , left zero
            , property "z-index" "5"
            , displayFlex
            , flexDirection column
            , alignItems center
            , paddingTop (px 300)
            ]
        , (.) SearchBar
            [ backgroundColor (hex Colors.darkGray)
            , width (pct 50)
            , borderRadius (px 3)
            , position relative
            , overflow hidden
            , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.5)
            , property "z-index" "2"
            ]
        , (.) SearchBarIcon
            [ width (px 26)
            , height (px 26)
            , color (hex Colors.lightGray)
            , left (px 16)
            , position absolute
            , top (pct 50)
            , marginTop (px -13)
            ]
        , (.) SearchBarInput
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
        , (.) Results
            [ marginTop (px 16)
            , backgroundColor (hex Colors.mediumGray)
            , padding (px 16)
            , borderRadius (px 3)
            , width (pct 50)
            , boxShadow5 zero (px 2) (px 4) zero (rgba 0 0 0 0.5)
            , property "z-index" "2"
            ]
        , (.) ResultsItem
            [ backgroundColor (hex Colors.darkGray)
            , marginBottom (px 2)
            , property "justify-content" "space-between"
            , displayFlex
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
        , (.) ResultsItemInfo
            [ padding (px 16)
            , displayFlex
            , alignItems center
            ]
        , (.) ResultsItemName
            [ fontSize (px 24)
            , lineHeight (px 30)
            , color (hex Colors.white)
            , paddingRight (px 8)
            ]
        , (.) ResultsItemVersion
            [ color (hex Colors.lightGray)
            , fontSize (px 20)
            , lineHeight (px 26)
            ]
        , (.) ResultsItemButtonGroup
            [ displayFlex
            ]
        , (.) ResultsItemButton
            [ property "background" "none"
            , border zero
            , height (pct 100)
            , displayFlex
            , flexDirection column
            , alignItems center
            , property "justify-content" "center"
            , color (hex Colors.lightGray)
            , borderLeft3 (px 2) solid (hex Colors.mediumGray)
            , width (px 48)
            , cursor pointer
            , textDecoration none
            ]
        , (.) ResultsItemButtonIcon
            [ width (px 16)
            , height (px 16)
            , display block
            ]
        , (.) ResultsItemButtonText
            [ fontSize (px 8)
            , textTransform uppercase
            , lineHeight (px 10)
            , paddingTop (px 2)
            ]
        , (.) Backdrop
            [ position absolute
            , width (pct 100)
            , height (pct 100)
            , top zero
            , left zero
            , property "z-index" "1"
            , backgroundColor <|
                rgba
                    (.r Colors.lightGrayRgb)
                    (.g Colors.lightGrayRgb)
                    (.b Colors.lightGrayRgb)
                    0.75
            ]
        ]
