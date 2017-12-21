module Pages.Editor.View exposing (view)

import Data.Ellie.SaveState as SaveState
import Ellie.Ui.Toast as Toast
import Extra.Html as Html
import Html exposing (Html, button, div, header, iframe, main_, span, text)
import Pages.Editor.Header.View as Header
import Pages.Editor.Layout.View as Layout
import Pages.Editor.Logs.View as Logs
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Output.View as Output
import Pages.Editor.Routing as Routing exposing (..)
import Pages.Editor.Save.Update as UpdateSave
import Pages.Editor.Sidebar.View as Sidebar
import Pages.Editor.Update as Update exposing (Msg(..))
import Pages.Editor.View.Styles as Styles
import RemoteData exposing (RemoteData(..))


viewHeader : Model -> Html Msg
viewHeader model =
    Header.view
        { onSave = SaveMsg UpdateSave.Start
        , onCompile = CompileRequested
        , onFormat = FormattingRequested
        , model = model.header
        , mapMsg = HeaderMsg
        , revisionId = model.clientRevision.id
        , embedLinkButtonEnabled =
            Routing.isSpecificRevision model.currentRoute
        , saveButtonEnabled =
            Model.canSave model
        , compileButtonEnabled =
            Model.canCompile model
        , buttonsVisible =
            RemoteData.isSuccess model.serverRevision
                && model.isOnline
        , termsState =
            case model.saveState of
                SaveState.AwaitingTermsAcceptance ->
                    Header.Visible
                        (SaveMsg << UpdateSave.TermsAcceptanceStart)
                        model.latestTermsVersion

                SaveState.AcceptingTerms ->
                    Header.Accepting model.latestTermsVersion

                _ ->
                    Header.Accepted
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
    Output.view
        { onClearElmStuff = ClearElmStuff
        , stage = model.compileStage
        , onCompile = CompileRequested
        }


viewSidebar : Model -> Html Msg
viewSidebar model =
    Sidebar.view
        { title = model.clientRevision.title
        , description = model.clientRevision.description
        , vimMode = model.vimMode
        , onTitleChange = TitleChanged
        , onDescriptionChange = DescriptionChanged
        , onClearElmStuff = ClearElmStuff
        , onVimModeChange = ToggleVimMode
        , installed = model.clientRevision.packages
        , onPackageRemoved = RemovePackageRequested
        , onPackageAdded = PackageSelected
        , latestTerms = model.latestTermsVersion
        , mapMsg = SidebarMsg
        , model = model.sidebar
        }


viewNotifications : Model -> Html Msg
viewNotifications model =
    Html.viewIf (not (List.isEmpty model.notifications)) <|
        div [ Styles.notifications ] <|
            List.map
                (\notification ->
                    Toast.view
                        { notification = notification
                        , onClose = ClearNotification notification
                        }
                )
                model.notifications


view : Model -> Html Msg
view model =
    Layout.view
        { header = viewHeader model
        , sidebar = viewSidebar model
        , elmId = "elmEditor"
        , htmlId = "htmlEditor"
        , output = viewOutput model
        , notifications = viewNotifications model
        , mapMsg = LayoutMsg
        , model = model.layout
        , logs = Html.map LogsMsg <| Logs.view model.logs
        , loading =
            RemoteData.isLoading model.serverRevision
                || RemoteData.isNotAsked model.serverRevision
        }
