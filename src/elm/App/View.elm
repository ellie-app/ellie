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


results : Revision -> RemoteData ApiError (List CompileError) -> Session -> Html Msg
results revision compileResult session =
    div
        [ class [ ResultsContainer ]
        , style [ ( "width", "50%" ) ]
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


editors : List CompileError -> Revision -> Session -> Html Msg
editors compileErrors revision session =
    div
        [ class [ EditorsContainer ]
        , style [ ( "width", "50%" ) ]
        ]
        [ div
            [ class [ EditorContainer ]
            , style
                [ ( "height", "50%" )
                , ( "border-bottom", "1px solid #bdb7bd" )
                ]
            ]
            [ Editors.elm
                ElmCodeChanged
                (revision.elmCode)
                (compileErrors)
            ]
        , div
            [ class [ EditorContainer ]
            , style [ ( "height", "50%" ) ]
            ]
            [ Editors.html
                HtmlCodeChanged
                (revision.htmlCode)
            ]
        ]


workArea : Model -> Html Msg
workArea model =
    div [ class [ WorkAreaContainer ] ]
        [ model.session
            |> RemoteData.map (editors (RemoteData.withDefault [] model.compileResult) model.clientRevision)
            |> RemoteData.withDefault htmlNone
        , model.session
            |> RemoteData.map (results model.clientRevision model.compileResult)
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
    , saveButtonEnabled =
        (Model.isRevisionChanged model || not (Model.isSavedProject model))
            && not (RemoteData.isLoading model.saveState)
            && Maybe.withDefault False model.isOnline
    , saveButtonOption =
        headerSaveOption model
    , compileButtonEnabled =
        not (RemoteData.isLoading model.compileResult)
            && RemoteData.isSuccess model.session
            && RemoteData.isSuccess model.serverRevision
            && ((not model.firstCompileComplete) || model.elmCodeChanged)
            && Maybe.withDefault False model.isOnline
    , buttonsVisible =
        RemoteData.isSuccess model.session
            && RemoteData.isSuccess model.serverRevision
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
    }


view : Model -> Html Msg
view model =
    div [ class [ TopContainer ] ]
        [ Header.view (headerContext model)
        , main_ [ class [ MainContainer ] ]
            [ mainStuff model
            ]
        ]
