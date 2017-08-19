module Pages.Editor.View exposing (view)

import Data.Ellie.CompileStage as CompileStage exposing (CompileStage)
import Extra.Html as Html
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
import Views.Editor.Terms.View as Terms
import Views.Editors.View as Editors
import Views.Output.View as Output


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


viewPopout : Model -> Html Msg
viewPopout model =
    case model.popoutState of
        AboutOpen ->
            About.view

        EmbedLinkOpen ->
            case model.currentRoute of
                SpecificRevision revisionId ->
                    EmbedLink.view
                        { projectId = revisionId.projectId
                        , revisionNumber = revisionId.revisionNumber
                        , onGist = CreateGist
                        , gistButtonEnabled = not model.creatingGist
                        }

                _ ->
                    Html.none

        TermsOpen ->
            Terms.view
                { termsVersion = model.latestTermsVersion
                , onAccept = TermsAcceptanceStart model.latestTermsVersion
                }

        AllClosed ->
            Html.none


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


viewSearch : Model -> Html Msg
viewSearch model =
    Html.viewIfLazy
        model.searchOpen
        (\_ ->
            Search.view
                { onClose = ToggleSearch
                , searchValue = model.searchValue
                , onSearchChange = SearchChanged
                , results = model.searchResults
                , onInstall = PackageSelected
                , packages = model.clientRevision.packages
                }
        )


viewWorkArea : Model -> Html Msg
viewWorkArea model =
    div [ class [ WorkArea ] ]
        [ viewEditors model
        , div
            [ class [ OutputResizeHandle ]
            , style [ ( "left", Utils.numberToPercent model.resultSplit ) ]
            , onMouseDown ResultDragStarted
            ]
            []
        , viewOutput model
        ]


viewEditors : Model -> Html Msg
viewEditors model =
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
            [ Html.viewIfLazy
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
        , Html.viewIf
            (not (Model.elmIsHidden model) && not (Model.htmlIsHidden model))
            (div
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
            [ Html.viewIfLazy
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


viewHeader : Model -> Html Msg
viewHeader model =
    Header.view
        { onSave = SaveRequested
        , onCompile = CompileRequested
        , onFormat = FormattingRequested
        , onAbout = ToggleAbout
        , onEmbedLink = ToggleEmbedLink
        , embedLinkButtonEnabled =
            Routing.isSpecificRevision model.currentRoute
        , saveButtonEnabled =
            Model.canSave model
        , compileButtonEnabled =
            Model.canCompile model
        , buttonsVisible =
            RemoteData.isSuccess model.serverRevision
                && model.isOnline
        , saveButtonOption =
            if RemoteData.isLoading model.saveState then
                Header.Saving
            else if Model.isOwnedProject model && Model.isSavedProject model then
                Header.Update
            else if Model.isOwnedProject model && not (Model.isSavedProject model) then
                Header.Save
            else
                Header.Fork
        }


viewOutput : Model -> Html Msg
viewOutput model =
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
            [ viewHeader model
            , div [ class [ MainContainer ] ]
                [ Sidebar.view <| sidebarContext model
                , viewWorkArea model
                , viewPopout model
                , Notifications.view
                    { notifications = model.notifications
                    , onClose = ClearNotification
                    }
                ]
            , viewSearch model
            ]
        ]
