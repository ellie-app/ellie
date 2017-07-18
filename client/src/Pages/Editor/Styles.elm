module Pages.Editor.Styles exposing (styles)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Namespace exposing (..)
import Pages.Editor.Classes exposing (Classes(..))
import Shared.Colors as Colors
import Shared.Constants as Constants


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
        , Css.class AppContainer
            [ width (pct 100)
            , height (pct 100)
            , displayFlex
            , position relative
            , backgroundColor (hex Colors.mediumGray)
            ]
        , Css.class WorkArea
            [ property "width" <| "calc(100% - " ++ toString Constants.sidebarWidth ++ "px)"
            , height (pct 100)
            , displayFlex
            , position relative
            ]
        , Css.class MainContainer
            [ width (pct 100)
            , property "height" <| "calc(100% - " ++ toString Constants.headerHeight ++ "px)"
            , displayFlex
            , position relative
            , property "z-index" "1"
            ]
        , Css.class AppContainerInner
            [ position relative
            , width (pct 100)
            , height (pct 100)
            , property "transition" "filter 0.3s 0.2s"
            , withClass LoadingRevision
                [ property "filter" "blur(30px)"
                ]
            ]
        , Css.class EditorsContainer
            [ displayFlex
            , position relative
            , property "z-index" "0"
            , flexDirection column
            , height (pct 100)
            , width (pct 50)
            , borderRight3 (px 2) solid (hex Colors.mediumGray)
            , overflow hidden
            ]
        , Css.class EditorContainer
            [ height (pct 50)
            , position relative
            , backgroundColor (hex Colors.darkGray)
            , firstChild
                [ borderBottom3 (px 1) solid (hex Colors.mediumGray)
                ]
            , lastChild
                [ borderTop3 (px 1) solid (hex Colors.mediumGray)
                ]
            , withClass EditorContainerCollapse
                [ height (px 40)
                ]
            , withClass EditorContainerFull
                [ property "height" "calc(100% - 40px)"
                ]
            ]
        , Css.class OutputContainer
            [ width (pct 50)
            , height (pct 100)
            , position relative
            , property "z-index" "1"
            , boxShadow5 (px -2) zero (px 8) zero (rgba 0 0 0 0.5)
            , overflow hidden
            , hover
                [ descendants
                    [ Css.class ReloadButton
                        [ display unset ]
                    , Css.class DebugButton
                        [ display unset ]
                    ]
                ]
            ]
        , Css.class NotificationsContainer
            [ position absolute
            , right (px 16)
            , top (px 16)
            ]
        , Css.class OutputResizeHandle
            [ position absolute
            , width (px 6)
            , height (pct 100)
            , marginLeft (px -3)
            , cursor ewResize
            , property "z-index" "6"
            ]
        , Css.class EditorResizeHandle
            [ position absolute
            , height (px 6)
            , width (pct 100)
            , marginTop (px -3)
            , cursor nsResize
            , property "z-index" "6"
            ]
        , Css.class ResizeNs
            [ descendants [ everything [ cursor nsResize |> important ] ]
            , cursor nsResize
            ]
        , Css.class ResizeEw
            [ descendants [ everything [ cursor ewResize |> important ] ]
            , cursor ewResize
            ]
        , Css.class EmbedLinkContainer
            [ position absolute
            , property "z-index" "2"
            , width (px 320)
            , left (px 448)
            , top (px 16)
            ]
        , Css.class OverlayButton
            [ position absolute
            , property "background" "none"
            , border zero
            , color (hex Colors.white)
            , displayFlex
            , property "z-index" "7"
            , cursor pointer
            , borderRadius (px 3)
            , padding (px 6)
            , backgroundColor <|
                rgba
                    (.r Colors.darkGrayRgb)
                    (.g Colors.darkGrayRgb)
                    (.b Colors.darkGrayRgb)
                    0.5
            , withClass CollapseButton
                [ top (px 6)
                , right (px 6)
                ]
            , withClass ReloadButton
                [ bottom (px 6)
                , right (px 6)
                ]
            , withClass DebugButton
                [ bottom (px 6)
                , left (px 6)
                ]
            ]
        , Css.class ReloadButton
            [ display none
            ]
        , Css.class DebugButton
            [ display none
            ]
        , Css.class OverlayButtonText
            [ fontSize (px 16)
            , textTransform uppercase
            , lineHeight (px 16)
            , adjacentSiblings
                [ Css.class OverlayButtonIcon
                    [ marginLeft (px 6)
                    ]
                ]
            ]
        , Css.class OverlayButtonIcon
            [ display block
            , width (px 16)
            , height (px 16)
            ]
        ]
