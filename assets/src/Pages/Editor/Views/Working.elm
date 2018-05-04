module Pages.Editor.Views.Working exposing (view)

import Colors
import Css exposing (..)
import Css.Foreign
import Data.Entity as Entity
import Data.Replaceable as Replaceable
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.SplitPane as SplitPane
import Ellie.Ui.Theme as Theme
import Extra.Html as Html
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Pages.Editor.State.Actions as ActionsState
import Pages.Editor.State.Working as WorkingState
import Pages.Editor.Views.Editors as EditorsView
import Pages.Editor.Views.Notifications as NotificationsView
import Pages.Editor.Views.Packages as PackagesView
import Pages.Editor.Views.Settings as SettingsView
import Pages.Editor.Views.Setup as SetupView
import Pages.Editor.Views.Sidebar as SidebarView
import Pages.Editor.Views.StatusBar as StatusBarView
import Pages.Editor.Views.Workbench as WorkbenchView


view : WorkingState.Model -> Html WorkingState.Msg
view model =
    Html.main_
        [ css
            [ position relative
            , height (pct 100)
            ]
        ]
        [ viewStyles model
        , Html.viewIf model.animating <|
            Html.div
                [ css
                    [ position absolute
                    , width (pct 100)
                    , height (pct 100)
                    , zIndex (int 1)
                    , backgroundColor Colors.darkGray
                    , property "animation-name" "setup-vanish"
                    , property "animation-duration" "0.4s"
                    , property "animation-delay" "0.6s"
                    , property "animation-fill-mode" "normal"
                    ]
                ]
                [ SetupView.view SetupView.Opening
                ]
        , Html.div
            [ css
                [ displayFlex
                , height (pct 100)
                ]
            ]
            [ SidebarView.view model.actions
            , Html.div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , height (pct 100)
                    , width (pct 100)
                    , position relative
                    , overflow hidden
                    ]
                ]
                [ Html.div
                    [ css
                        [ displayFlex
                        , position relative
                        , width (pct 100)
                        , height (pct 100)
                        ]
                    ]
                    [ viewActions model
                    , viewWorkspace model
                    , NotificationsView.view
                        { onClose = WorkingState.CloseNotification
                        , onCloseAll = WorkingState.CloseAllNotifications
                        , notifications = model.notifications
                        }
                    ]
                , StatusBarView.view
                    { connected = model.connected
                    , notificationCount = List.length model.notifications
                    , compileStatus =
                        case ( model.compiling, model.workbench ) of
                            ( True, _ ) ->
                                "Compiling..."

                            ( False, WorkingState.Ready ) ->
                                "Ready"

                            ( False, WorkingState.FinishedWithError { error } ) ->
                                "Compiled with errors"

                            ( False, WorkingState.Finished _ ) ->
                                "Compiled successfully"
                    }
                ]
            ]
        ]


viewActions : WorkingState.Model -> Html WorkingState.Msg
viewActions model =
    case model.actions of
        ActionsState.Hidden ->
            Html.text ""

        _ ->
            Html.div
                [ css
                    [ width (px 360)
                    , borderRight3 (px 2) solid Theme.staticBorder
                    , flexShrink (int 0)
                    ]
                ]
                [ case model.actions of
                    ActionsState.Packages packagesModel ->
                        PackagesView.view
                            { query = packagesModel.query
                            , onSearch = WorkingState.ActionsMsg << ActionsState.UserTypedInPackageSearch
                            , installedPackages = model.packages
                            , searchedPackages = packagesModel.searchedPackages
                            , isLoading = packagesModel.awaitingSearch
                            , onUninstall = WorkingState.PackageUninstalled
                            , onInstall = WorkingState.PackageInstalled
                            }

                    ActionsState.Settings ->
                        SettingsView.view
                            { onSettingsChange = WorkingState.SettingsChanged
                            , settings = model.user.settings
                            , onProjectNameChange = WorkingState.ChangedProjectName
                            , projectName = model.projectName
                            }

                    _ ->
                        Html.text ""
                ]


viewWorkspace : WorkingState.Model -> Html WorkingState.Msg
viewWorkspace model =
    SplitPane.view
        { direction = SplitPane.Horizontal
        , ratio = model.workbenchRatio
        , originalRatio = 0.5
        , onResize = WorkingState.WorkbenchResized
        , minSize = 24
        , first = viewEditors model
        , second = viewWorkbench model
        }


viewEditors : WorkingState.Model -> Html WorkingState.Msg
viewEditors model =
    EditorsView.view
        { elmCode = model.elmCode
        , analysis = model.analysis
        , onElmChange = WorkingState.ElmCodeChanged
        , onTokenChange = WorkingState.TokenChanged
        , onAdvancedTokenChange = WorkingState.AdvancedTokenChanged
        , htmlCode = model.htmlCode
        , onHtmlChange = WorkingState.HtmlCodeChanged
        , onResize = WorkingState.EditorsResized
        , onExampleSelect = WorkingState.ExampleSelected
        , ratio = model.editorsRatio
        , onFormat = WorkingState.FormatRequested
        , onCollapse = WorkingState.CollapseHtml
        , vimMode = model.user.settings.vimMode
        , elmError =
            case model.workbench of
                WorkingState.FinishedWithError { error } ->
                    Just error

                _ ->
                    Nothing
        }


viewWorkbench : WorkingState.Model -> Html WorkingState.Msg
viewWorkbench model =
    WorkbenchView.view
        { onExpand = WorkingState.ExpandWorkbench
        , onIframeReload = WorkingState.IframeReloadClicked
        , onLogSearchChanged = WorkingState.LogSearchChanged
        , onClearLogs = WorkingState.ClearLogsClicked
        , onDownloadZip = WorkingState.DownloadZip
        , onLogReceived = WorkingState.LogReceived
        , onSelectSuccessPane = WorkingState.SuccessPaneSelected
        , onSelectErrorsPane = WorkingState.ErrorsPaneSelected
        , onGoToLocation = WorkingState.LocationSelected
        , onCanDebugChange = WorkingState.CanDebugUpdated
        , onCompile =
            if model.connected then
                Just WorkingState.CompileRequested
            else
                Nothing
        , onSave =
            if model.connected && WorkingState.hasChanged model then
                Just WorkingState.SaveRequested
            else
                Nothing
        , compilerVersion = WorkingState.compilerVersion model
        , compiling = model.compiling
        , saving = model.saving
        , workbench = model.workbench
        , maximized = model.workbenchRatio < 0.05
        , htmlCode = model.htmlCode
        , token = model.token
        , revisionId = Maybe.map Tuple.first <| Replaceable.toMaybe model.revision
        }


viewStyles model =
    Css.Foreign.global
        [ Css.Foreign.selector ":root"
            [ property "--editor-font-size" model.user.settings.fontSize
            , property "--theme-font-family-editor" model.user.settings.fontFamily
            ]
        ]
