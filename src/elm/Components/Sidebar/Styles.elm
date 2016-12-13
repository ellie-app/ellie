module Components.Sidebar.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Css.Elements exposing (..)
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
        ]
