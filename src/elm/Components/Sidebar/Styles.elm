module Components.Sidebar.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Components.Sidebar.Classes exposing (..)
import Shared.Constants as Constants


styles : Stylesheet
styles =
    (stylesheet << namespace "components_sidebar_")
        [ (.) Sidebar
            [ height (pct 100)
            , width (px Constants.sidebarWidth)
            ]
        , (.) Section
            []
        , (.) SectionHeader
            [ fontSize (px 13)
            , textTransform uppercase
            , margin zero
            , fontWeight (int 300)
            , padding2 (px 8) (px 16)
            , backgroundColor (hex "f7f7f7")
            , borderTop3 (px 1) solid (hex "bdb7bd")
            , borderBottom3 (px 1) solid (hex "bdb7bd")
            ]
        , (.) SectionContent
            [ padding (px 16)
            ]
        , (.) TextInput
            [ property "-webkit-appearance" "none"
            , border zero
            , outline zero
            , width (pct 100)
            , borderBottom3 (px 2) solid (hex "bdb7bd")
            , height (px 32)
            , fontSize (px 16)
            , fontFamily inherit
            , fontWeight (int 300)
            , focus
                [ borderBottomColor (hex "55B5DB")
                ]
            ]
        , (.) Textarea
            [ property "-webkit-appearance" "none"
            , border zero
            , outline zero
            , width (pct 100)
            , borderBottom3 (px 2) solid (hex "bdb7bd")
            , height (px 32)
            , fontSize (px 16)
            , fontFamily inherit
            , fontWeight (int 300)
            , property "resize" "none"
            , height (px 120)
            , focus
                [ borderBottomColor (hex "55B5DB")
                ]
            ]
        , (.) PackagesList
            [ maxHeight (px 300)
            , overflowY auto
            , padding2 (px 8) (px 16)
            , borderBottom3 (px 2) solid (hex "f7f7f7")
            ]
        , (.) DepItem
            [ padding2 (px 8) zero
            , displayFlex
            , property "align-items" "center"
            , property "justify-content" "space-between"
            ]
        , (.) DepItemDetails
            [ property "width" "calc(100% - 32px)"
            , fontSize (px 16)
            , fontFamilies [ "monospace" ]
            ]
        , (.) DepItemPackageName
            [ whiteSpace noWrap
            , textOverflow ellipsis
            , width (pct 100)
            , overflow hidden
            , fontWeight bold
            ]
        , (.) IconButton
            [ width (px 32)
            , height (px 32)
            , property "background" "none"
            , border zero
            , cursor pointer
            ]
        , (.) AddDepButton
            [ width (pct 100)
            , height (px 48)
            , border zero
            , cursor pointer
            , backgroundColor (hex "E5E1DB")
            , fontFamily inherit
            , fontSize (px 14)
            , lineHeight (px 14)
            , textTransform uppercase
            , textAlign center
            ]
        , (.) AddDepButtonText
            [ display inlineBlock
            , verticalAlign middle
            ]
        , (.) AddDepButtonIcon
            [ width (px 16)
            , height (px 16)
            , display inlineBlock
            , marginLeft (px 8)
            , verticalAlign middle
            ]
        , (.) AddDepContainer
            [ displayFlex
            ]
        , (.) AddDepInputContainer
            [ property "width" "calc(100% - 32px)"
            ]
        , (.) AddDepResultList
            [ position absolute
            , width (pct 100)
            , marginTop (px 12)
            , boxShadow5 (px 4) (px 4) (px 15) (px -4) (rgba 0 0 0 0.5)
            , padding2 (px 8) zero
            ]
        , (.) AddDepPackageSearch
            [ position relative
            ]
        , (.) AddDepPackageItem
            [ cursor pointer
            , padding2 (px 8) (px 16)
            , hover
                [ backgroundColor (hex "f7f7f7")
                ]
            ]
        , (.) AddDepVersion
            [ displayFlex
            , property "align-items" "center"
            , property "justify-content" "space-between"
            ]
        , (.) AddDepVersionDetails
            [ property "width" "calc(100% - 64px)"
            , fontSize (px 16)
            , fontFamilies [ "monospace" ]
            ]
        , (.) AddDepVersionPackageName
            [ whiteSpace noWrap
            , textOverflow ellipsis
            , width (pct 100)
            , overflow hidden
            , fontWeight bold
            ]
        ]
