module Pages.Editor.View exposing (view)

import Data.Ellie.CompileStage as CompileStage exposing (CompileStage)
import Data.Ellie.Notification as Notification
import Data.Ellie.SaveState as SaveState
import Extra.Html as Html
import Extra.Html.Attributes as Attributes
import Html exposing (Html, button, div, header, iframe, main_, span, text)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick, onMouseDown)
import Pages.Editor.Model as Model exposing (Model, PopoutState(..))
import Pages.Editor.Routing as Routing exposing (..)
import Pages.Editor.Update as Update exposing (Msg(..))
import Pages.Editor.Update.Save as UpdateSave
import Pages.Editor.View.Styles as Styles
import RemoteData exposing (RemoteData(..))
import Shared.Icons as Icons
import Shared.Utils as Utils
import Views.Editor.About as About
import Views.Editor.EmbedLink as EmbedLink
import Views.Editor.Header as Header
import Views.Editor.Notifications as Notifications
import Views.Editor.Search as Search
import Views.Editor.Sidebar as Sidebar
import Views.Editor.Terms as Terms
import Views.Editors as Editors
import Views.Output as Output


attrWhen : Html.Attribute msg -> Bool -> Html.Attribute msg
attrWhen attr cond =
    if cond then
        attr
    else
        Attributes.none


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
                { latestTermsVersion = model.latestTermsVersion
                }

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
                , onAccept = SaveMsg <| UpdateSave.TermsAcceptanceStart model.latestTermsVersion
                }

        AllClosed ->
            Html.none


sidebarContext : Model -> Sidebar.Config Msg
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
    div [ Styles.workArea ]
        [ viewEditors model
        , div
            [ Styles.outputResizeHandle
            , style [ ( "left", Utils.numberToPercent model.resultSplit ) ]
            , onMouseDown ResultDragStarted
            ]
            []
        , viewOutput model
        ]


viewEditors : Model -> Html Msg
viewEditors model =
    div
        [ Styles.editorsContainer
        , style [ ( "width", Utils.numberToPercent model.resultSplit ) ]
        ]
        [ div
            [ Styles.editorContainer
            , attrWhen Styles.editorContainerCollapse <| Model.elmIsHidden model
            , attrWhen Styles.editorContainerFull <| Model.htmlIsHidden model
            , style
                [ ( "height"
                  , elmHeightCss model
                  )
                ]
            ]
            [ div
                [ id "elmEditor"
                , style <|
                    if Model.elmIsHidden model then
                        [ ( "display", "none" ) ]
                    else
                        []
                ]
                []
            , button
                [ Styles.overlayButton
                , Styles.collapseButton
                , onClick ToggleElmCollapse
                ]
                [ span [ Styles.overlayButtonText ] [ text "Elm" ]
                , span [ Styles.overlayButtonIcon ] [ expandButtonIcon <| Model.elmIsHidden model ]
                ]
            ]
        , Html.viewIf
            (not (Model.elmIsHidden model) && not (Model.htmlIsHidden model))
            (div
                [ Styles.editorResizeHandle
                , style [ ( "top", elmHeightCss model ) ]
                , onMouseDown EditorDragStarted
                ]
                []
            )
        , div
            [ Styles.editorContainer
            , attrWhen Styles.editorContainerCollapse <| Model.htmlIsHidden model
            , attrWhen Styles.editorContainerFull <| Model.elmIsHidden model
            , style
                [ ( "height"
                  , htmlHeightCss model
                  )
                ]
            ]
            [ div
                [ id "htmlEditor"
                , style <|
                    if Model.htmlIsHidden model then
                        [ ( "display", "none" ) ]
                    else
                        []
                ]
                []
            , button
                [ Styles.overlayButton
                , Styles.collapseButton
                , onClick ToggleHtmlCollapse
                ]
                [ span [ Styles.overlayButtonText ] [ text "HTML" ]
                , span [ Styles.overlayButtonIcon ] [ expandButtonIcon <| Model.htmlIsHidden model ]
                ]
            ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    Header.view
        { onSave = SaveMsg UpdateSave.Start
        , onCompile = CompileRequested
        , onFormat = FormattingRequested
        , onAbout = TogglePopouts AboutOpen
        , onEmbedLink = TogglePopouts EmbedLinkOpen
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
            if SaveState.isWorking model.saveState then
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
        [ Styles.outputContainer
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
        [ Styles.appContainer
        , attrWhen Styles.resizeEw model.resultDragging
        , attrWhen Styles.resizeNs model.editorDragging
        ]
        [ div
            [ Styles.appContainerInner
            , attrWhen Styles.loadingRevision <|
                RemoteData.isLoading model.serverRevision
                    || RemoteData.isNotAsked model.serverRevision
            ]
            [ viewHeader model
            , div [ Styles.mainContainer ]
                [ Sidebar.view <| sidebarContext model
                , viewWorkArea model
                , viewPopout model
                , Notifications.view
                    { notifications = model.notifications
                    , onClose = ClearNotification
                    , onAction =
                        \action ->
                            case action of
                                Notification.ClearElmStuff ->
                                    ClearElmStuff
                    }
                ]
            , viewSearch model
            ]
        ]
