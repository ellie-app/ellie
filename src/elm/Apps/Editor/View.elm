module Apps.Editor.View exposing (view)

import Html exposing (Html, div, button, text, iframe, main_, header, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown)
import RemoteData exposing (RemoteData(..))
import Types.Revision as Revision exposing (Revision)
import Types.ApiError as ApiError exposing (ApiError)
import Types.CompileError as CompileError exposing (CompileError)
import Types.Session as Session exposing (Session)
import Types.ProjectId as ProjectId exposing (ProjectId)
import Apps.Editor.Update as Update exposing (Msg(..))
import Apps.Editor.Model as Model exposing (Model, PopoutState(..))
import Apps.Editor.Classes exposing (..)
import Apps.Editor.Routing as Routing exposing (..)
import Components.Splash.View as Splash
import Components.Sidebar.View as Sidebar
import Components.Editors.View as Editors
import Components.Header.View as Header
import Components.Output.View as Output
import Components.Notifications.View as Notifications
import Components.About.View as About
import Components.Search.View as Search
import Components.Editor.EmbedLink.View as EmbedLink
import Shared.Utils as Utils


renderIf : Bool -> (() -> Html msg) -> Html msg
renderIf predicate thunk =
    if predicate then
        thunk ()
    else
        text ""


popoutView : Model -> Html Msg
popoutView model =
    case model.popoutState of
        NotificationsOpen ->
            renderIf
                (not <| List.isEmpty model.notifications)
                (\_ -> Notifications.view model.notifications)

        AboutOpen ->
            About.view

        _ ->
            text ""


sidebarContext : Model -> Sidebar.ViewModel Msg
sidebarContext model =
    { title = model.clientRevision.title
    , description = model.clientRevision.description
    , onTitleChange = TitleChanged
    , onDescriptionChange = DescriptionChanged
    , dependencies = model.clientRevision.dependencies
    , onAddPackageClick = ToggleSearch
    , installingPackage = model.installingPackage
    , removingHashes = model.removingDependencyHashes
    , onRemove = RemoveDependencyRequested
    }


searchContext : Model -> Search.ViewModel Msg
searchContext model =
    { onClose = ToggleSearch
    , searchValue = model.searchValue
    , onSearchChange = SearchChanged
    , results = model.searchResults
    , onInstall = PackageSelected
    , dependencies = model.clientRevision.dependencies
    }


embedLink : Model -> Html msg
embedLink model =
    case ( model.popoutState, model.currentRoute ) of
        ( EmbedLinkOpen, SpecificRevision projectId revisionNumber ) ->
            div
                [ class [ EmbedLinkContainer ] ]
                [ EmbedLink.view
                    { projectId = ProjectId.toEncodedString projectId
                    , revisionNumber = revisionNumber
                    }
                ]

        _ ->
            text ""


loadedView : Model -> Html Msg
loadedView model =
    div [ class [ LoadedContainer ] ]
        [ Header.view <| headerContext model
        , div [ class [ MainContainer ] ]
            [ Sidebar.view <| sidebarContext model
            , workAreaView model
            , div [ class [ NotificationsContainer ] ]
                [ popoutView model ]
            , embedLink model
            ]
        , renderIf
            model.searchOpen
            (\_ -> Search.view <| searchContext model)
        ]


workAreaView : Model -> Html Msg
workAreaView model =
    div [ class [ WorkArea ] ]
        [ editorsView model
        , div
            [ class [ OutputResizeHandle ]
            , style [ ( "left", Utils.numberToPercent model.resultSplit ) ]
            , onMouseDown ResultDragStarted
            ]
            []
        , model.session
            |> RemoteData.map (outputView model)
            |> RemoteData.withDefault (text "")
        ]


editorsView : Model -> Html Msg
editorsView model =
    div
        [ class [ EditorsContainer ]
        , style [ ( "width", Utils.numberToPercent model.resultSplit ) ]
        ]
        [ div
            [ class [ EditorContainer ]
            , style [ ( "height", Utils.numberToPercent model.editorSplit ) ]
            ]
            [ Editors.elm
                model.vimMode
                (Just ElmCodeChanged)
                model.stagedElmCode
                (model.compileResult |> RemoteData.withDefault [])
            ]
        , div
            [ class [ EditorResizeHandle ]
            , style [ ( "top", Utils.numberToPercent model.editorSplit ) ]
            , onMouseDown EditorDragStarted
            ]
            []
        , div
            [ class [ EditorContainer ]
            , style
                [ ( "height", Utils.numberToPercent (1 - model.editorSplit) )
                ]
            ]
            [ Editors.html
                model.vimMode
                (Just HtmlCodeChanged)
                model.stagedHtmlCode
            ]
        ]


headerSaveOption : Model -> Header.SaveOption
headerSaveOption model =
    if Model.isOwnedProject model && Model.isSavedProject model then
        Header.Update
    else if Model.isOwnedProject model && not (Model.isSavedProject model) then
        Header.Save
    else
        Header.Fork


headerContext : Model -> Header.ViewModel Msg
headerContext model =
    { onSave = SaveRequested
    , onCompile = CompileRequested
    , onFormat = FormattingRequested
    , onAbout = ToggleAbout
    , onEmbedLink = ToggleEmbedLink
    , onNotifications = ToggleNotifications
    , notificationCount = model.unseenNotificationsCount
    , embedLinkButtonEnabled =
        Routing.isSpecificRevision model.currentRoute
    , saveButtonEnabled =
        Model.canSave model
    , saveButtonOption =
        headerSaveOption model
    , compileButtonEnabled =
        Model.canCompile model
    , buttonsVisible =
        RemoteData.isSuccess model.session
            && RemoteData.isSuccess model.serverRevision
            && model.isOnline
    }


outputView : Model -> Session -> Html Msg
outputView model session =
    div
        [ class [ OutputContainer ]
        , style [ ( "width", Utils.numberToPercent (1 - model.resultSplit) ) ]
        ]
        [ case RemoteData.map2 (\a _ -> a) model.compileResult model.iframeResult of
            Success errors ->
                case List.filter (.level >> (==) "error") errors of
                    [] ->
                        Output.success session.id

                    relevantErrors ->
                        Output.errors relevantErrors

            Failure _ ->
                Output.failure

            Loading ->
                Output.compiling

            NotAsked ->
                Output.waiting
        ]


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( AppContainer, True )
            , ( ResizeEw, model.resultDragging )
            , ( ResizeNs, model.editorDragging )
            ]
        ]
        [ case model.session of
            Success _ ->
                loadedView model

            Failure _ ->
                loadedView model

            _ ->
                Splash.view
        ]
