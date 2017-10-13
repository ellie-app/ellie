module Pages.Editor.Layout.View exposing (..)

import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Extra.Html as Html
import Extra.Html.Attributes as Attributes exposing (style)
import Html exposing (Html, aside, div, header, main_)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseDown)
import Pages.Editor.Layout.Model as Model exposing (DragTarget(..), Model)
import Pages.Editor.Layout.Styles as Styles
import Pages.Editor.Layout.Update exposing (Msg(..))


type alias Config msg =
    { header : Html msg
    , sidebar : Html msg
    , output : Html msg
    , loading : Bool
    , notifications : Html msg
    , model : Model
    , mapMsg : Msg -> msg
    , elmId : String
    , htmlId : String
    , logs : Html msg
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
    div [ Styles.collapseButton ]
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
        [ Styles.editorsContainer
        , style "width" <| numberToPercent config.model.resultSplit
        ]
        [ div
            [ Styles.editorContainer
            , Attributes.cond Styles.editorContainerCollapse <| config.model.editorCollapse == Model.JustHtmlOpen
            , Attributes.cond Styles.editorContainerFull <| config.model.editorCollapse == Model.JustElmOpen
            , style "height" <| elmHeightCss config.model
            ]
            [ div
                [ id config.elmId
                , Attributes.cond (style "display" "none") <| config.model.editorCollapse == Model.JustHtmlOpen
                ]
                []
            , viewCollapseButton
                (config.mapMsg <| ToggleEditorCollapse Model.JustHtmlOpen)
                (config.model.editorCollapse == Model.JustHtmlOpen)
                "Elm"
            ]
        , Html.viewIf (config.model.editorCollapse == Model.BothOpen) <|
            div
                [ Styles.verticalResizeHandle
                , style "top" <| elmHeightCss config.model
                , onMouseDown (config.mapMsg EditorDragStarted)
                ]
                []
        , div
            [ Styles.editorContainer
            , Attributes.cond Styles.editorContainerCollapse <| config.model.editorCollapse == Model.JustElmOpen
            , Attributes.cond Styles.editorContainerFull <| config.model.editorCollapse == Model.JustHtmlOpen
            , style "height" <| htmlHeightCss config.model
            ]
            [ div
                [ id config.htmlId
                , Attributes.cond (style "display" "none") <| config.model.editorCollapse == Model.JustElmOpen
                ]
                []
            , viewCollapseButton
                (config.mapMsg <| ToggleEditorCollapse Model.JustElmOpen)
                (config.model.editorCollapse == Model.JustElmOpen)
                "HTML"
            ]
        ]


viewOutputAndLogs : Config msg -> Html msg
viewOutputAndLogs config =
    div
        [ Styles.outputAndLogsContainer
        , style "width" <| numberToPercent (1 - config.model.resultSplit)
        ]
        [ div
            [ style "height" <| outputHeightCss config.model
            , Styles.outputContainer
            ]
            [ config.output ]
        , Html.viewIf (not config.model.logsCollapsed) <|
            div
                [ Styles.verticalResizeHandle
                , style "top" <| outputHeightCss config.model
                , onMouseDown (config.mapMsg LogsDragStarted)
                ]
                []
        , div
            [ style "height" <| logsHeightCss config.model
            , Styles.logsContainer
            , Attributes.cond Styles.logsContainerCollapsed config.model.logsCollapsed
            ]
            [ Html.viewIf (not config.model.logsCollapsed) <|
                div [] [ config.logs ]
            , viewCollapseButton
                (config.mapMsg <| ToggleLogsCollapse)
                config.model.logsCollapsed
                "Logs"
            ]
        ]


view : Config msg -> Html msg
view config =
    div
        [ Styles.appContainer
        , Attributes.cond Styles.resizeEw <|
            config.model.dragTarget
                == OutputHandle
        , Attributes.cond Styles.resizeNs <|
            (config.model.dragTarget == EditorsHandle)
                || (config.model.dragTarget == LogsHandle)
        ]
        [ div
            [ Styles.appContainerInner
            , Attributes.cond Styles.loadingRevision config.loading
            ]
            [ header [ Styles.header ] [ config.header ]
            , div [ Styles.mainContainer ]
                [ aside [ Styles.sidebar ] [ config.sidebar ]
                , main_ [ Styles.workArea ]
                    [ viewEditors config
                    , div
                        [ Styles.outputResizeHandle
                        , style "left" <| numberToPercent config.model.resultSplit
                        , onMouseDown (config.mapMsg ResultDragStarted)
                        ]
                        []
                    , viewOutputAndLogs config
                    , div
                        [ Styles.notifications ]
                        [ config.notifications
                        ]
                    ]
                ]
            ]
        ]
