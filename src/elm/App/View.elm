module App.View exposing (view)

import Html exposing (Html, div, button, text, iframe, main_, header, span)
import Html.Attributes exposing (value, style, srcdoc)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import RemoteData exposing (RemoteData(..))
import RemoteData.Infix exposing ((<*>))
import Types.ApiError as ApiError exposing (ApiError)
import Types.CompileError as CompileError exposing (CompileError)
import Types.Revision as Revision exposing (Revision)
import Types.Session as Session exposing (Session)
import Shared.Icons as Icons
import Shared.Utils as Utils
import Shared.Selector as Selector
import App.Update as Update exposing (Msg(..), NewPackageFlowMsg(..))
import App.Model as Model exposing (Model)
import App.Classes exposing (..)
import Components.Header.View as Header
import Components.Sidebar.View as Sidebar
import Components.Editors.View as Editors
import Components.Output.View as Output


when : Bool -> (() -> Html msg) -> Html msg
when pred thunk =
    if pred then
        thunk ()
    else
        htmlNone


htmlNone : Html msg
htmlNone =
    text ""


mainStuffError : String -> Html msg
mainStuffError message =
    div []
        [ text <| "Failed: " ++ message
        ]


loadingSession : Html msg
loadingSession =
    div [ class [ MainLoadingMessageContainer ] ]
        [ div [ class [ MainLoadingMessageHeader ] ]
            [ text "Preparing your session..." ]
        , div [ class [ MainLoadingMessageElmLogo ] ]
            [ Icons.elmLogo
            ]
        ]


loadingRevision : Html msg
loadingRevision =
    div [ class [ MainLoadingMessageContainer ] ]
        [ div [ class [ MainLoadingMessageHeader ] ]
            [ text "Loading your project..." ]
        ]


results : Float -> Revision -> RemoteData ApiError (List CompileError) -> Session -> Html Msg
results split revision compileResult session =
    div
        [ class [ ResultsContainer ]
        , style [ ( "width", Utils.numberToPercent (1 - split) ) ]
        ]
        [ case compileResult of
            Success errors ->
                case List.filter (.level >> (==) "error") errors of
                    [] ->
                        Output.success session.id revision.htmlCode

                    relevantErrors ->
                        Output.errors relevantErrors

            Failure _ ->
                div [] [ text "no!" ]

            Loading ->
                Output.compiling

            NotAsked ->
                Output.waiting
        ]


editors : Float -> Float -> List CompileError -> String -> String -> Session -> Html Msg
editors editorSplit resultSplit compileErrors htmlCode elmCode session =
    div
        [ class [ EditorsContainer ]
        , style [ ( "width", Utils.numberToPercent resultSplit ) ]
        ]
        [ div
            [ class [ EditorContainer ]
            , style
                [ ( "height", Utils.numberToPercent editorSplit )
                , ( "border-bottom", "1px solid #bdb7bd" )
                ]
            ]
            [ Editors.elm
                ElmCodeChanged
                (elmCode)
                (compileErrors)
            ]
        , div
            [ onMouseDown EditorDragStarted
            , style
                [ ( "width", "100%" )
                , ( "height", "5px" )
                , ( "position", "absolute" )
                , ( "top", "calc(" ++ Utils.numberToPercent editorSplit ++ " - 2px)" )
                , ( "z-index", "4" )
                , ( "cursor", "ns-resize" )
                ]
            ]
            []
        , div
            [ class [ EditorContainer ]
            , style [ ( "height", Utils.numberToPercent (1 - editorSplit) ) ]
            ]
            [ Editors.html
                HtmlCodeChanged
                (htmlCode)
            ]
        ]


workArea : Model -> Html Msg
workArea model =
    div [ class [ WorkAreaContainer ] ]
        [ model.session
            |> RemoteData.map
                (editors
                    model.editorSplit
                    model.resultSplit
                    (RemoteData.withDefault [] model.compileResult)
                    model.stagedHtmlCode
                    model.stagedElmCode
                )
            |> RemoteData.withDefault htmlNone
        , div
            [ onMouseDown ResultDragStarted
            , style
                [ ( "height", "100%" )
                , ( "width", "5px" )
                , ( "position", "absolute" )
                , ( "left", "calc(" ++ Utils.numberToPercent model.resultSplit ++ " - 2px)" )
                , ( "z-index", "4" )
                , ( "cursor", "ew-resize" )
                ]
            ]
            []
        , model.session
            |> RemoteData.map (results model.resultSplit model.clientRevision model.compileResult)
            |> RemoteData.withDefault htmlNone
        ]


ready : Model -> Html Msg
ready model =
    div [ class [ MainContainerInner ] ]
        [ Sidebar.view <| sidebarContext model
        , workArea model
        ]


mainStuff : Model -> Html Msg
mainStuff model =
    case Success (,) <*> model.session <*> model.serverRevision of
        Success ( session, serverRevision ) ->
            ready model

        Failure error ->
            mainStuffError error.message

        Loading ->
            if RemoteData.isLoading model.session then
                loadingSession
            else
                loadingRevision

        NotAsked ->
            mainStuffError "This shouldn't have happened"


headerSaveOption : Model -> Header.SaveOption
headerSaveOption model =
    if Model.isOwnedProject model && Model.isSavedProject model then
        Header.Update
    else if Model.isOwnedProject model && not (Model.isSavedProject model) then
        Header.Save
    else
        Header.Fork


headerContext : Model -> Header.Context Msg
headerContext model =
    { onSave = SaveRequested
    , onCompile = CompileRequested
    , onFormat = FormattingRequested
    , onNotificationsToggled = ToggleNotifications
    , notifications = model.notifications
    , notificationsOpen = model.notificationsOpen
    , notificationsHighlight = model.notificationsHighlight
    , saveButtonEnabled =
        (Model.isRevisionChanged model || not (Model.isSavedProject model))
            && not (RemoteData.isLoading model.saveState)
            && model.isOnline
    , saveButtonOption =
        headerSaveOption model
    , compileButtonEnabled =
        Model.canCompile model
    , buttonsVisible =
        RemoteData.isSuccess model.session
            && RemoteData.isSuccess model.serverRevision
            && model.isOnline
    }


sidebarContext : Model -> Sidebar.Context Msg
sidebarContext model =
    { dependencies = model.clientRevision.dependencies
    , newPackageFlow = model.newPackageFlow
    , onStarted = NewPackageFlowMsg Started
    , onSearchTermChanged = SearchTermUpdated >> NewPackageFlowMsg
    , onPackageSelected = PackageSelected >> NewPackageFlowMsg
    , onVersionSelected = VersionSelected >> NewPackageFlowMsg
    , onInstallRequested = NewPackageFlowMsg InstallRequested
    , onCancelled = NewPackageFlowMsg Cancelled
    , onRemoved = RemoveDependencyRequested
    , title = model.clientRevision.title
    , onTitleChanged = TitleChanged
    , description = model.clientRevision.description
    , onDescriptionChanged = DescriptionChanged
    }


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( TopContainer, True )
            , ( TopContainerDragging, model.resultDragging || model.editorDragging )
            , ( TopContainerNs, model.editorDragging )
            , ( TopContainerEw, model.resultDragging )
            ]
        ]
        [ Header.view (headerContext model)
        , main_ [ class [ MainContainer ] ]
            [ mainStuff model
            ]
        ]
