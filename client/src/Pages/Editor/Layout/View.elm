module Pages.Editor.Layout.View exposing (..)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Extra.Css exposing (blur, filter)
import Extra.Html as Html
import Extra.Html.Attributes as Attributes exposing (style)
import Html.Styled exposing (Attribute, Html, aside, div, header, main_)
import Html.Styled.Attributes exposing (css, id)
import Html.Styled.Events exposing (onMouseDown)
import Pages.Editor.Layout.Model as Model exposing (DragTarget(..), Model)
import Pages.Editor.Layout.Update exposing (Msg(..))


type alias Config msg =
    { header : Html msg
    , sidebar : Html msg
    , output : Html msg
    , loading : Bool
    , notifications : Html msg
    , model : Model
    , mapMsg : Msg -> msg
    , elmEditor : Html msg
    , htmlEditor : Html msg
    , logs : Html msg
    , styles : Html msg
    }


numberToPercent : number -> String
numberToPercent number =
    toString (number * 100) ++ "%"


htmlHeightCss : Model -> String
htmlHeightCss model =
    case model.editorCollapse of
        Model.BothOpen ->
            numberToPercent (1 - model.editorSplit)

        _ ->
            ""


elmHeightCss : Model -> String
elmHeightCss model =
    case model.editorCollapse of
        Model.BothOpen ->
            numberToPercent model.editorSplit

        _ ->
            ""


logsHeightCss : Model -> String
logsHeightCss model =
    if not model.logsCollapsed then
        numberToPercent (1 - model.logsSplit)
    else
        ""


outputHeightCss : Model -> String
outputHeightCss model =
    if not model.logsCollapsed then
        numberToPercent model.logsSplit
    else
        ""


viewCollapseButton : msg -> Bool -> String -> Html msg
viewCollapseButton msg collapsed label =
    div [ collapseButtonStyles ]
        [ Button.view
            { label = label
            , disabled = False
            , size = Button.Medium
            , style = Button.Link
            , attributes = []
            , action = Button.click msg
            , icon =
                if collapsed then
                    Just Icon.Unfold
                else
                    Just Icon.Fold
            }
        ]


viewEditors : Config msg -> Html msg
viewEditors config =
    div
        [ editorsContainerStyles
        , style "width" <| numberToPercent config.model.resultSplit
        ]
        [ div
            [ editorContainerStyles
            , Attributes.cond editorContainerCollapseStyles <| config.model.editorCollapse == Model.JustHtmlOpen
            , Attributes.cond editorContainerFullStyles <| config.model.editorCollapse == Model.JustElmOpen
            , style "height" <| elmHeightCss config.model
            ]
            [ div
                [ Attributes.cond (style "display" "none") <| config.model.editorCollapse == Model.JustHtmlOpen ]
                [ config.elmEditor ]
            , viewCollapseButton
                (config.mapMsg <| ToggleEditorCollapse Model.JustHtmlOpen)
                (config.model.editorCollapse == Model.JustHtmlOpen)
                "Elm"
            ]
        , Html.viewIf (config.model.editorCollapse == Model.BothOpen) <|
            div
                [ verticalResizeHandleStyles
                , style "top" <| elmHeightCss config.model
                , onMouseDown (config.mapMsg EditorDragStarted)
                ]
                []
        , div
            [ editorContainerStyles
            , Attributes.cond editorContainerCollapseStyles <| config.model.editorCollapse == Model.JustElmOpen
            , Attributes.cond editorContainerFullStyles <| config.model.editorCollapse == Model.JustHtmlOpen
            , style "height" <| htmlHeightCss config.model
            ]
            [ div
                [ Attributes.cond (style "display" "none") <| config.model.editorCollapse == Model.JustElmOpen ]
                [ config.htmlEditor ]
            , viewCollapseButton
                (config.mapMsg <| ToggleEditorCollapse Model.JustElmOpen)
                (config.model.editorCollapse == Model.JustElmOpen)
                "HTML"
            ]
        ]


viewOutputAndLogs : Config msg -> Html msg
viewOutputAndLogs config =
    div
        [ outputAndLogsContainerStyles
        , style "width" <| numberToPercent (1 - config.model.resultSplit)
        ]
        [ div
            [ style "height" <| outputHeightCss config.model
            , outputContainerStyles
            ]
            [ config.output ]
        , Html.viewIf (not config.model.logsCollapsed) <|
            div
                [ verticalResizeHandleStyles
                , style "top" <| outputHeightCss config.model
                , onMouseDown (config.mapMsg LogsDragStarted)
                ]
                []
        , div
            [ style "height" <| logsHeightCss config.model
            , logsContainerStyles
            , Attributes.cond logsContainerCollapsedStyles config.model.logsCollapsed
            ]
            [ Html.viewIf (not config.model.logsCollapsed) <| config.logs
            , viewCollapseButton
                (config.mapMsg <| ToggleLogsCollapse)
                config.model.logsCollapsed
                "Logs"
            ]
        ]


view : Config msg -> Html msg
view config =
    div
        [ appContainerStyles
        , Attributes.cond resizeEwStyles <|
            config.model.dragTarget
                == OutputHandle
        , Attributes.cond resizeNsStyles <|
            (config.model.dragTarget == EditorsHandle)
                || (config.model.dragTarget == LogsHandle)
        ]
        [ config.styles
        , div
            [ appContainerInnerStyles
            , Attributes.cond loadingRevisionStyles config.loading
            ]
            [ header [ headerStyles ] [ config.header ]
            , div [ mainContainerStyles ]
                [ aside [ sidebarStyles ] [ config.sidebar ]
                , main_ [ workAreaStyles ]
                    [ viewEditors config
                    , div
                        [ outputResizeHandleStyles
                        , style "left" <| numberToPercent config.model.resultSplit
                        , onMouseDown (config.mapMsg ResultDragStarted)
                        ]
                        []
                    , viewOutputAndLogs config
                    , div
                        [ notificationsStyles ]
                        [ config.notifications
                        ]
                    ]
                ]
            ]
        ]



-- STYLES


notificationsStyles : Attribute msg
notificationsStyles =
    css
        [ position absolute
        , bottom zero
        , right zero
        , width (px 400)
        , zIndex (int 10)
        , maxHeight (pct 100)
        , overflowY auto
        ]


sidebarStyles : Attribute msg
sidebarStyles =
    css
        [ width (px 240)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        , Colors.boxShadow |> .right
        ]


headerStyles : Attribute msg
headerStyles =
    css
        [ height (px 40)
        ]


appContainerStyles : Attribute msg
appContainerStyles =
    css
        [ width (pct 100)
        , height (pct 100)
        , displayFlex
        , position relative
        ]


workAreaStyles : Attribute msg
workAreaStyles =
    css
        [ width <| calc (pct 100) minus (px 240)
        , height (pct 100)
        , displayFlex
        , position relative
        ]


mainContainerStyles : Attribute msg
mainContainerStyles =
    css
        [ width (pct 100)
        , height <| calc (pct 100) minus (px 40)
        , displayFlex
        , position relative
        , zIndex (int 1)
        ]


appContainerInnerStyles : Attribute msg
appContainerInnerStyles =
    css
        [ position relative
        , width (pct 100)
        , height (pct 100)
        , property "transition" "filter 0.3s 0.2s"
        ]


loadingRevisionStyles : Attribute msg
loadingRevisionStyles =
    css
        [ filter <| blur (px 30)
        ]


editorsContainerStyles : Attribute msg
editorsContainerStyles =
    css
        [ displayFlex
        , position relative
        , zIndex (int 0)
        , flexDirection column
        , height (pct 100)
        , width (pct 50)
        , overflow hidden
        ]


editorContainerStyles : Attribute msg
editorContainerStyles =
    css
        [ height (pct 50)
        , position relative
        , backgroundColor Colors.darkMediumGray
        , firstChild
            [ borderBottom3 (px 1) solid Colors.darkGray
            ]
        , lastChild
            [ borderTop3 (px 1) solid Colors.darkGray
            ]
        ]


editorContainerCollapseStyles : Attribute msg
editorContainerCollapseStyles =
    css
        [ height (px 40)
        ]


editorContainerFullStyles : Attribute msg
editorContainerFullStyles =
    css
        [ height <| calc (pct 100) minus (px 40)
        ]


outputAndLogsContainerStyles : Attribute msg
outputAndLogsContainerStyles =
    css
        [ width (pct 50)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        , Colors.boxShadow |> .left
        , overflow hidden
        , displayFlex
        , flexDirection column
        ]


outputContainerStyles : Attribute msg
outputContainerStyles =
    css
        [ height (pct 100)
        , flexShrink (int 1)
        , position relative
        , displayFlex
        ]


logsContainerStyles : Attribute msg
logsContainerStyles =
    css
        [ position relative
        , displayFlex
        , borderTop3 (px 2) solid Colors.mediumGray
        ]


logsContainerCollapsedStyles : Attribute msg
logsContainerCollapsedStyles =
    css
        [ height (px 40)
        , flexShrink (int 0)
        ]


notificationsContainerStyles : Attribute msg
notificationsContainerStyles =
    css
        [ position absolute
        , right (px 16)
        , top (px 16)
        ]


outputResizeHandleStyles : Attribute msg
outputResizeHandleStyles =
    css
        [ position absolute
        , width (px 6)
        , height (pct 100)
        , marginLeft (px -3)
        , cursor ewResize
        , zIndex (int 6)
        ]


verticalResizeHandleStyles : Attribute msg
verticalResizeHandleStyles =
    css
        [ position absolute
        , height (px 6)
        , width (pct 100)
        , marginTop (px -3)
        , cursor nsResize
        , zIndex (int 6)
        ]


resizeNsStyles : Attribute msg
resizeNsStyles =
    css
        [ Css.Foreign.descendants [ Css.Foreign.everything [ cursor nsResize |> important ] ]
        , cursor nsResize
        ]


resizeEwStyles : Attribute msg
resizeEwStyles =
    css
        [ Css.Foreign.descendants [ Css.Foreign.everything [ cursor ewResize |> important ] ]
        , cursor ewResize
        ]


collapseButtonStyles : Attribute msg
collapseButtonStyles =
    css
        [ top (px 8)
        , right (px 12)
        , position absolute
        , zIndex (int 2)
        ]
