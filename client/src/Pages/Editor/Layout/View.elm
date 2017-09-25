module Pages.Editor.Layout.View exposing (..)

import Extra.Html as Html
import Extra.Html.Attributes as Attributes exposing (style)
import Html exposing (Html, aside, div, header, main_)
import Html.Attributes exposing (id)
import Html.Events exposing (onMouseDown)
import Pages.Editor.Layout.Model as Model exposing (Model)
import Pages.Editor.Layout.Styles as Styles
import Pages.Editor.Layout.Update exposing (Msg(..))
import Shared.Utils as Utils


type alias Config msg =
    { header : Html msg
    , sidebar : Html msg
    , output : Html msg
    , loading : Bool
    , model : Model
    , mapMsg : Msg -> msg
    , elmId : String
    , htmlId : String
    }


htmlHeightCss : Model -> String
htmlHeightCss model =
    case model.editorCollapse of
        Model.BothOpen ->
            Utils.numberToPercent (1 - model.editorSplit)

        _ ->
            ""


elmHeightCss : Model -> String
elmHeightCss model =
    case model.editorCollapse of
        Model.BothOpen ->
            Utils.numberToPercent model.editorSplit

        _ ->
            ""


viewEditors : Config msg -> Html msg
viewEditors config =
    div
        [ Styles.editorsContainer
        , style "width" <| Utils.numberToPercent config.model.resultSplit
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
            ]
        , Html.viewIf (config.model.editorCollapse == Model.BothOpen) <|
            div
                [ Styles.editorResizeHandle
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
            ]
        ]


view : Config msg -> Html msg
view config =
    div
        [ Styles.appContainer
        , Attributes.cond Styles.resizeEw config.model.resultDragging
        , Attributes.cond Styles.resizeNs config.model.editorDragging
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
                        , style "left" <| Utils.numberToPercent config.model.resultSplit
                        , onMouseDown (config.mapMsg ResultDragStarted)
                        ]
                        []
                    , div
                        [ Styles.outputContainer
                        , style "width" <| Utils.numberToPercent (1 - config.model.resultSplit)
                        ]
                        [ config.output ]
                    ]
                ]
            ]
        ]
