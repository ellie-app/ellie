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
            [ displayFlex
            , flexDirection column
            ]
        , (.) SectionHeader
            [ padding (px 16)
            , color (hex "fff")
            , backgroundColor (hex "55B5DB")
            , displayFlex
            , property "justify-content" "space-between"
            , cursor pointer
            ]
        , (.) SectionHeaderIcon
            [ width (px 20)
            , height (px 20)
            , property "fill" "currentColor"
            ]
        , (.) SectionHeaderText
            [ textTransform uppercase
            , fontSize (px 18)
            ]
        , (.) Details
            [ padding (px 16)
            ]
        , (.) DetailsInputContainer
            [ padding2 (px 8) zero
            ]
        , (.) DetailsTitle
            [ property "-webkit-appearance" "none"
            , border zero
            , borderBottom3 (px 1) solid (hex "A7A7A7")
            , outline zero
            ]
        , (.) Dependencies
            [ displayFlex
            , flexDirection column
            , padding2 zero (px 16)
            ]
        , (.) InstalledDeps
            [ padding2 (px 8) zero
            ]
        , (.) InstalledDepContainer
            [ padding2 (px 8) zero
            , width (pct 100)
            , displayFlex
            ]
        , (.) InstalledDepDetails
            [ property "width" "calc(100% - 32px)" ]
        , (.) InstalledDepName
            [ property "font-family" "monospace"
            ]
        , (.) InstalledDepRange
            [ property "font-family" "monospace"
            ]
        , (.) InstalledDepMin
            []
        , (.) InstalledDepMax
            []
        , (.) AddDepButton
            [ property "-webkit-appearance" "none"
            , backgroundColor (hex "A7A7A7")
            , color (hex "fff")
            , property "border" "none"
            , padding (px 12)
            , outline zero
            , cursor pointer
            ]
        , (.) AddDepButtonText
            [ fontSize (px 13)
            , marginLeft (px 8)
            , textTransform uppercase
            , display inlineBlock
            , verticalAlign middle
            ]
        , (.) AddDepButtonIcon
            [ width (px 13)
            , height (px 13)
            , display inlineBlock
            , verticalAlign middle
            ]
        , (.) CloseButton
            [ width (px 32)
            , height (px 32)
            , property "-webkit-appearance" "none"
            , property "background" "none"
            , property "border" "none"
            , cursor pointer
            , padding (px 8)
            , outline zero
            ]
        ]
