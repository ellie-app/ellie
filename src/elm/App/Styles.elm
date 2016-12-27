module App.Styles exposing (styles)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (..)
import App.Classes exposing (Classes(..))
import Shared.Constants as Constants


styles : Stylesheet
styles =
    (stylesheet << namespace "app_")
        [ html
            [ height (pct 100) ]
        , body
            [ height (pct 100)
            , margin zero
            , property "font-family" "sans-serif"
            , fontWeight (int 300)
            ]
        , everything
            [ boxSizing borderBox ]
        , (.) EditorsContainer
            [ displayFlex
            , flexDirection column
            , borderRight3 (px 1) solid (hex "dddddd")
            , height (pct 100)
            ]
        , (.) EditorContainer
            [ width (pct 100)
            , overflow hidden
            ]
        , (.) EditorsSeparator
            [ width (pct 100)
            , height (px 5)
            , top (px -2)
            , marginBottom (px -5)
            , property "z-index" "3"
            , cursor nsResize
            ]
        , (.) WorkAreaContainer
            [ displayFlex
            , height (pct 100)
            , property "width" <| "calc(100% - " ++ toString Constants.sidebarWidth ++ "px)"
            , position relative
            ]
        , (.) ResultsEditorsSeparator
            [ position relative
            , height (pct 100)
            , width (px 5)
            , right (px -2)
            , marginLeft (px -5)
            , cursor ewResize
            , property "z-index" "3"
            ]
        , (.) ResultsContainer
            [ displayFlex
            , height (pct 100)
            ]
        , (.) MainContainer
            [ property "height" ("calc(100% - " ++ toString Constants.headerHeight ++ "px)")
            , position relative
            , displayFlex
            ]
        , (.) TopContainer
            [ height (pct 100)
            , position relative
            ]
        , (.) TopContainerNs
            [ descendants
                [ everything [ cursor nsResize |> important ] ]
            ]
        , (.) TopContainerEw
            [ descendants
                [ everything [ cursor ewResize |> important ] ]
            ]
        , (.) TopContainerDragging
            [ descendants
                [ everything [ property "user-select" "none" ] ]
            ]
        , (.) MainLoadingMessageContainer
            [ width (pct 100)
            , height (pct 100)
            , backgroundColor (hex "55B5DB")
            , displayFlex
            , property "align-items" "center"
            , property "justify-content" "center"
            , flexDirection column
            ]
        , (.) MainLoadingMessageHeader
            [ fontSize (px 56)
            , color (hex "fff")
            , property "font-family" Constants.scriptFont
            , textShadow4 (px 1) (px 2) (px 6) (rgba 0 0 0 0.5)
            , marginBottom (px 56)
            ]
        , (.) MainLoadingMessageElmLogo
            [ color (hex "fff")
            , height (px 120)
            , textShadow4 (px 1) (px 2) (px 6) (rgba 0 0 0 0.5)
            , property "filter" "drop-shadow( 1px 2px 6px rgba(0,0,0,0.5) )"
            ]
        , (.) MainContainerInner
            [ displayFlex
            , width (pct 100)
            , height (pct 100)
            ]
        ]
