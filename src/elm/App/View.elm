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
            case ( model.session, model.compileResult ) of
                ( Success session, Success _ ) ->
                    Output.success session.id model.htmlCode

                ( Success _, NotAsked ) ->
                    Output.waiting

                ( Success _, Loading ) ->
                    Output.compiling

                ( Success _, _ ) ->
                    div [] [ text "eh" ]

                ( Failure _, _ ) ->
                    div [] [ text "ugh" ]

                ( Loading, _ ) ->
                    Output.loading

                ( NotAsked, _ ) ->
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
    , saveButtonEnabled = True
    , compileButtonEnabled =
        (not (RemoteData.isLoading model.compileResult))
            && (RemoteData.isSuccess model.session)
    , onSave = NoOp
    , onCompile = Compile
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
