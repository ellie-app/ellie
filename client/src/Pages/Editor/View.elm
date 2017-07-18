module Pages.Editor.View exposing (view)

import Data.Ellie.CompileStage as CompileStage exposing (CompileStage)
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Html exposing (Html, button, div, header, iframe, main_, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown)
import Pages.Editor.Classes exposing (..)
import Pages.Editor.Model as Model exposing (Model, PopoutState(..))
import Pages.Editor.Routing as Routing exposing (..)
import Pages.Editor.Update as Update exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Shared.Icons as Icons
import Shared.Utils as Utils
import Views.Editor.About.View as About
import Views.Editor.EmbedLink.View as EmbedLink
import Views.Editor.Header.View as Header
import Views.Editor.Notifications.View as Notifications
import Views.Editor.Search.View as Search
import Views.Editor.Sidebar.View as Sidebar
import Views.Editors.View as Editors
import Views.Output.View as Output


renderIf : Bool -> (() -> Html msg) -> Html msg
renderIf predicate thunk =
    if predicate then
        thunk ()
    else
        text ""


htmlHeightCss : Model -> String
htmlHeightCss model =
    if Model.htmlIsHidden model || Model.elmIsHidden model then
        ""
    else
        Utils.numberToPercent (1 - model.editorSplit)


elmHeightCss : Model -> String
elmHeightCss model =
    if Model.htmlIsHidden model || Model.elmIsHidden model then
        ""
    else
        Utils.numberToPercent model.editorSplit


expandButtonIcon : Bool -> Html msg
expandButtonIcon isHidden =
    if isHidden then
        Icons.expand
    else
        Icons.collapse


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
    , packages = model.clientRevision.packages
    , onAddPackageClick = ToggleSearch
    , onRemove = RemovePackageRequested
    }


searchContext : Model -> Search.ViewModel Msg
searchContext model =
    { onClose = ToggleSearch
    , searchValue = model.searchValue
    , onSearchChange = SearchChanged
    , results = model.searchResults
    , onInstall = PackageSelected
    , packages = model.clientRevision.packages
    }


embedLinkVm : RevisionId -> Model -> EmbedLink.ViewModel Msg
embedLinkVm { projectId, revisionNumber } model =
    { projectId = projectId
    , revisionNumber = revisionNumber
    , onGist = CreateGist
    , gistButtonEnabled = not model.creatingGist
    }


embedLink : Model -> Html Msg
embedLink model =
    case ( model.popoutState, model.currentRoute ) of
        ( EmbedLinkOpen, SpecificRevision revisionId ) ->
            div
                [ class [ EmbedLinkContainer ] ]
                [ EmbedLink.view <| embedLinkVm revisionId model
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
        , outputView model
        ]


editorsView : Model -> Html Msg
editorsView model =
    div
        [ class [ EditorsContainer ]
        , style [ ( "width", Utils.numberToPercent model.resultSplit ) ]
        ]
        [ div
            [ classList
                [ ( EditorContainer, True )
                , ( EditorContainerCollapse, Model.elmIsHidden model )
                , ( EditorContainerFull, Model.htmlIsHidden model )
                ]
            , style
                [ ( "height"
                  , elmHeightCss model
                  )
                ]
            ]
            [ renderIf
                (not <| Model.elmIsHidden model)
                (\_ ->
                    Editors.elm
                        model.vimMode
                        (Just ElmCodeChanged)
                        model.stagedElmCode
                        (Model.compileErrors model)
                )
            , button
                [ class [ OverlayButton, CollapseButton ]
                , onClick ToggleElmCollapse
                ]
                [ span [ class [ OverlayButtonText ] ] [ text "Elm" ]
                , span [ class [ OverlayButtonIcon ] ] [ expandButtonIcon <| Model.elmIsHidden model ]
                ]
            ]
        , renderIf
            (not (Model.elmIsHidden model) && not (Model.htmlIsHidden model))
            (\_ ->
                div
                    [ class [ EditorResizeHandle ]
                    , style [ ( "top", elmHeightCss model ) ]
                    , onMouseDown EditorDragStarted
                    ]
                    []
            )
        , div
            [ classList
                [ ( EditorContainer, True )
                , ( EditorContainerCollapse, Model.htmlIsHidden model )
                , ( EditorContainerFull, Model.elmIsHidden model )
                ]
            , style
                [ ( "height"
                  , htmlHeightCss model
                  )
                ]
            ]
            [ renderIf
                (not <| Model.htmlIsHidden model)
                (\_ ->
                    Editors.html
                        model.vimMode
                        (Just HtmlCodeChanged)
                        model.stagedHtmlCode
                )
            , button
                [ class [ OverlayButton, CollapseButton ]
                , onClick ToggleHtmlCollapse
                ]
                [ span [ class [ OverlayButtonText ] ] [ text "HTML" ]
                , span [ class [ OverlayButtonIcon ] ] [ expandButtonIcon <| Model.htmlIsHidden model ]
                ]
            ]
        ]


headerSaveOption : Model -> Header.SaveOption
headerSaveOption model =
    if RemoteData.isLoading model.saveState then
        Header.Saving
    else if Model.isOwnedProject model && Model.isSavedProject model then
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
        True
    , buttonsVisible =
        RemoteData.isSuccess model.serverRevision
            && model.isOnline
    }


outputView : Model -> Html Msg
outputView model =
    div
        [ class [ OutputContainer ]
        , style [ ( "width", Utils.numberToPercent (1 - model.resultSplit) ) ]
        ]
        [ case model.compileStage of
            CompileStage.Initial ->
                Output.initial

            CompileStage.LoadingCompiler percentage ->
                Output.loadingCompiler percentage

            CompileStage.InstallingPackages ->
                Output.installing

            CompileStage.PlanningBuild ->
                Output.planning

            CompileStage.Compiling { total, complete } ->
                Output.compiling total complete

            CompileStage.GeneratingCode ->
                Output.generating

            CompileStage.Success url ->
                Output.success url

            CompileStage.FinishedWithErrors errors ->
                Output.errors errors

            CompileStage.Failed message ->
                Output.failure
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
        [ div
            [ classList
                [ ( AppContainerInner
                  , True
                  )
                , ( LoadingRevision
                  , RemoteData.isLoading model.serverRevision
                        || RemoteData.isNotAsked model.serverRevision
                  )
                ]
            ]
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
        ]
