module View exposing (view)

import Html exposing (Html, div, button, text, iframe, main_, header, span)
import Html.Attributes exposing (value, style, srcdoc)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import RemoteData exposing (RemoteData(..))
import Update exposing (Msg(..))
import Model exposing (Model)
import Views.Editors as Editors
import Views.Results as Results
import Views.Header as Header
import Classes exposing (..)


floatToPercentageString : Float -> String
floatToPercentageString float =
    toString (float * 100) ++ "%"


viewEditors : Model -> Html Msg
viewEditors model =
    let
        elmEditor =
            case model.session of
                Success _ ->
                    Editors.elm
                        (TypedElmCode)
                        (model.elmCode)
                        (model.compileResult |> RemoteData.withDefault [])

                Failure _ ->
                    div [] [ text "nope!" ]

                Loading ->
                    Editors.loading

                NotAsked ->
                    div [] [ text "yo" ]

        htmlEditor =
            case model.session of
                Success _ ->
                    Editors.html
                        (TypedHtmlCode)
                        (model.htmlCode)

                Failure _ ->
                    div [] [ text "nope!" ]

                Loading ->
                    Editors.loading

                NotAsked ->
                    div [] [ text "yo" ]
    in
        div
            [ class [ EditorsContainer ]
            , style [ ( "width", floatToPercentageString model.editorsResultSplit ) ]
            ]
            [ div
                [ class [ EditorContainer ]
                , style
                    [ ( "height", floatToPercentageString model.editorEditorSplit )
                    , ( "border-bottom", "1px solid #bdb7bd" )
                    ]
                ]
                [ elmEditor ]
            , div
                [ class [ EditorsSeparator ]
                , onMouseDown StartDraggingEditors
                ]
                []
            , div
                [ class [ EditorContainer ]
                , style [ ( "height", floatToPercentageString (1 - model.editorEditorSplit) ) ]
                ]
                [ htmlEditor ]
            ]


viewResults : Model -> Html Msg
viewResults model =
    let
        innerView =
            case model.session of
                Success _ ->
                    div [] [ text "ready to go" ]

                Failure _ ->
                    div [] [ text "ugh" ]

                Loading ->
                    Results.loading

                NotAsked ->
                    div [] [ text "waiting!" ]
    in
        div
            [ class [ ResultsContainer ]
            , style [ ( "width", floatToPercentageString (1 - model.editorsResultSplit) ) ]
            ]
            [ innerView ]


viewMain : Model -> Html Msg
viewMain model =
    div [ class [ WorkAreaContainer ] ]
        [ viewEditors model
        , div
            [ class [ ResultsEditorsSeparator ]
            , onMouseDown StartDraggingResult
            ]
            []
        , viewResults model
        ]


containerCursor : Model -> String
containerCursor model =
    if model.isDraggingEditorsSplit then
        "ns-resize"
    else if model.isDraggingResultSplit then
        "ew-resize"
    else
        ""


containerUserSelect : Model -> String
containerUserSelect model =
    if model.isDraggingEditorsSplit || model.isDraggingResultSplit then
        "none"
    else
        ""


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "height", "100%" )
            , ( "position", "relative" )
            , ( "cursor", containerCursor model )
            , ( "use-select", containerUserSelect model )
            ]
        ]
        [ Header.view
        , main_
            [ style
                [ ( "height", "calc(100% - 60px)" )
                , ( "position", "relative" )
                , ( "display", "flex" )
                ]
            ]
            [ viewMain model
            ]
        ]
