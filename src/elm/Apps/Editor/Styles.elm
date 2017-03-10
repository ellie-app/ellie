module Apps.Editor.Styles exposing (styles)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (..)
import Apps.Editor.Classes exposing (Classes(..))
import Shared.Constants as Constants
import Shared.Colors as Colors


styles : Stylesheet
styles =
    (stylesheet << namespace "app_")
        [ html
            [ height (pct 100)
            ]
        , body
            [ height (pct 100)
            , margin zero
            , fontFamilies [ Constants.sansFont ]
            , property "-webkit-font-smoothing" "antialiased"
            ]
        , everything
            [ boxSizing borderBox ]
        , button
            [ focus [ outline zero ] ]
        , input
            [ focus [ outline zero ] ]
        , (.) AppContainer
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , position relative
            , backgroundColor (hex Colors.mediumGray)
            ]
        , (.) WorkArea
            [ property "width" <| "calc(100% - " ++ toString Constants.sidebarWidth ++ "px)"
            , height (pct 100)
            , displayFlex
            , position relative
            ]
        , (.) MainContainer
            [ width (pct 100)
            , property "height" <| "calc(100% - " ++ (toString Constants.headerHeight) ++ "px)"
            , displayFlex
            , position relative
            , property "z-index" "1"
            ]
        , (.) AppContainerInner
            [ position relative
            , width (pct 100)
            , height (pct 100)
            , property "transition" "filter 0.3s 0.2s"
            , withClass LoadingRevision
                [ property "filter" "blur(30px)"
                ]
            ]
        , (.) EditorsContainer
            [ displayFlex
            , position relative
            , property "z-index" "0"
            , flexDirection column
            , height (pct 100)
            , width (pct 50)
            , borderRight3 (px 2) solid (hex Colors.mediumGray)
            ]
        , (.) EditorContainer
            [ height (pct 50)
            , firstChild
                [ borderBottom3 (px 1) solid (hex Colors.mediumGray)
                ]
            , lastChild
                [ borderTop3 (px 1) solid (hex Colors.mediumGray)
                ]
            ]
        , (.) OutputContainer
            [ width (pct 50)
            , height (pct 100)
            , position relative
            , property "z-index" "1"
            , boxShadow5 (px -2) zero (px 8) zero (rgba 0 0 0 0.5)
            , overflow hidden
            ]
        , (.) NotificationsContainer
            [ position absolute
            , right (px 16)
            , top (px 16)
            ]
        , (.) OutputResizeHandle
            [ position absolute
            , width (px 6)
            , height (pct 100)
            , marginLeft (px -3)
            , cursor ewResize
            , property "z-index" "6"
            ]
        , (.) EditorResizeHandle
            [ position absolute
            , height (px 6)
            , width (pct 100)
            , marginTop (px -3)
            , cursor nsResize
            , property "z-index" "6"
            ]
        , (.) ResizeNs
            [ descendants [ everything [ cursor nsResize |> important ] ]
            , cursor nsResize
            ]
        , (.) ResizeEw
            [ descendants [ everything [ cursor ewResize |> important ] ]
            , cursor ewResize
            ]
        , (.) EmbedLinkContainer
            [ position absolute
            , property "z-index" "2"
            , width (px 320)
            , left (px 448)
            , top (px 16)
            ]
        ]
