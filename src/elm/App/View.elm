module App.View exposing (view)

import Html exposing (Html, div, button, text, iframe, main_, header, span)
import Html.Attributes exposing (value, style, srcdoc)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import RemoteData exposing (RemoteData(..))
import App.Update exposing (Msg(..))
import App.Model exposing (Model)
import Components.Editors.View as Editors
import Components.Header.View as Header
import Components.Sidebar.View as Sidebar
import Components.Output.View as Output
import App.Classes exposing (..)


floatToPercentageString : Float -> String
floatToPercentageString float =
    toString (float * 100) ++ "%"


viewEditors : Model -> Html Msg
viewEditors model =
    let
        ( elmEditor, htmlEditor ) =
            case ( model.session, model.currentRevision ) of
                ( Loading, _ ) ->
                    ( Editors.loading, Editors.loading )

                ( _, Loading ) ->
                    ( Editors.loading, Editors.loading )

                ( Success _, _ ) ->
                    ( Editors.elm
                        (TypedElmCode)
                        (model.elmCode)
                        (model.compileResult |> RemoteData.withDefault [])
                    , Editors.html
                        (TypedHtmlCode)
                        (model.htmlCode)
                    )

                ( Failure _, _ ) ->
                    ( div [] [ text "nope!" ], div [] [ text "nope!" ] )

                _ ->
                    ( div [] [ text "yo" ], div [] [ text "yo!" ] )
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
            case ( model.session, model.compileResult, model.currentRevision ) of
                ( _, _, Loading ) ->
                    Output.loading

                ( Success session, Success _, _ ) ->
                    Output.success session.id model.htmlCode

                ( Success _, NotAsked, _ ) ->
                    Output.waiting

                ( Success _, Loading, _ ) ->
                    Output.compiling

                ( Success _, _, _ ) ->
                    div [] [ text "eh" ]

                ( Failure _, _, _ ) ->
                    div [] [ text "ugh" ]

                ( Loading, _, _ ) ->
                    Output.loading

                ( NotAsked, _, _ ) ->
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


headerContext : Model -> Header.Context Msg
headerContext model =
    { saveButtonOption = Header.Save
    , onSave = SaveButtonClicked
    , onCompile = Compile
    , saveButtonEnabled =
        not (RemoteData.isLoading model.currentRevision)
            && RemoteData.isSuccess model.session
    , compileButtonEnabled =
        not (RemoteData.isLoading model.compileResult)
            && RemoteData.isSuccess model.session
    }


sidebarContext : Model -> Sidebar.Context Msg
sidebarContext model =
    { detailsTitle = model.title
    , detailsDescription = model.description
    , onLocalMsg = SidebarMsg
    , onTitleChange = TitleChanged
    , onDescriptionChange = DescriptionChanged
    }


view : Model -> Html Msg
view model =
    div [ class [ TopContainer ] ]
        [ Header.view (headerContext model)
        , main_ [ class [ MainContainer ] ]
            [ Sidebar.view (sidebarContext model) model.sidebar
            , viewMain model
            ]
        ]
