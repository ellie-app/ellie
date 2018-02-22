module Pages.Editor.Views.Working exposing (view)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.SplitPane as SplitPane
import Extra.Html as Html
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Pages.Editor.State.Actions as ActionsState
import Pages.Editor.State.Working as WorkingState
import Pages.Editor.Views.Editors as EditorsView
import Pages.Editor.Views.Packages as PackagesView
import Pages.Editor.Views.Settings as SettingsView
import Pages.Editor.Views.Setup as SetupView
import Pages.Editor.Views.Sidebar as SidebarView
import Pages.Editor.Views.StatusBar as StatusBarView
import Pages.Editor.Views.Workbench as WorkbenchView


view : WorkingState.Model -> Html WorkingState.Msg
view model =
    Html.main_ [ css [ position relative ] ]
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
            [ css [ displayFlex ]
            ]
            [ SidebarView.view model.actions
            , Html.div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , height (pct 100)
                    , width (pct 100)
                    ]
                ]
                [ case model.actions of
                    ActionsState.Hidden ->
                        viewWorkspace model

                    _ ->
                        SplitPane.view
                            { direction = SplitPane.Horizontal
                            , ratio = model.actionsRatio
                            , originalRatio = 0.1
                            , onResize = WorkingState.ActionsResized
                            , minSize = 300
                            , first =
                                case model.actions of
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
                            , second =
                                viewWorkspace model
                            }
                , StatusBarView.view
                    { connected = model.connected
                    , compileStatus =
                        case model.compilation of
                            WorkingState.Ready ->
                                "Ready"

                            WorkingState.Compiling ->
                                "Compiling..."

                            WorkingState.FinishedWithErrors errors ->
                                toString (List.length errors)
                                    ++ " compiler error"
                                    ++ (if List.length errors > 1 then
                                            "s"
                                        else
                                            ""
                                       )

                            WorkingState.Succeeded ->
                                "Success"
                    }
                ]
            ]
        ]


viewWorkspace : WorkingState.Model -> Html WorkingState.Msg
viewWorkspace model =
    SplitPane.view
        { direction = SplitPane.Horizontal
        , ratio = model.workbenchRatio
        , originalRatio = 0.5
        , onResize = WorkingState.WorkbenchResized
        , minSize = 24
        , first =
            EditorsView.view
                { elmCode = model.elmCode
                , onElmChange = WorkingState.ElmCodeChanged
                , htmlCode = model.htmlCode
                , onHtmlChange = WorkingState.HtmlCodeChanged
                , ratio = model.editorsRatio
                , onResize = WorkingState.EditorsResized
                , vimMode = model.user.settings.vimMode
                , onFormat = WorkingState.FormatRequested
                , onCollapse = WorkingState.CollapseHtml
                , elmErrors = model.currentErrors
                }
        , second =
            WorkbenchView.view
                { onCompile = WorkingState.CompileRequested
                , onSelectPane = WorkingState.WorkbenchPaneSelected
                , compilation = model.compilation
                , pane = model.workbenchPane
                , token = model.token
                }
        }


viewStyles model =
    Css.Foreign.global
        [ Css.Foreign.selector ":root"
            [ property "--editor-font-size" model.user.settings.fontSize
            , property "--editor-font-family" model.user.settings.fontFamily
            ]
        ]
