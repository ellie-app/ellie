module Pages.Editor.Views.Working exposing (view)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Types.Settings as Settings
import Ellie.Ui.SplitPane as SplitPane
import Ellie.Ui.Theme as Theme
import Extra.Html as Html
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)
import Pages.Editor.State.Actions as ActionsState
import Pages.Editor.State.Working as WorkingState
import Pages.Editor.Views.Editors as EditorsView
import Pages.Editor.Views.Packages as PackagesView
import Pages.Editor.Views.Settings as SettingsView
import Pages.Editor.Views.Setup as SetupView
import Pages.Editor.Views.Sidebar as SidebarView


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
            , case model.actions of
                ActionsState.Hidden ->
                    viewWorkspace model

                _ ->
                    SplitPane.view
                        { direction = SplitPane.Horizontal
                        , ratio = model.actionsRatio
                        , onResize = WorkingState.ActionsResized
                        , first =
                            case model.actions of
                                ActionsState.Packages packagesModel ->
                                    Html.map WorkingState.ActionsMsg <|
                                        PackagesView.view
                                            { query = packagesModel.query
                                            , onSearch = ActionsState.UserTypedInPackageSearch
                                            , installedPackages = model.packages
                                            , searchedPackages = packagesModel.searchedPackages
                                            , isLoading = packagesModel.awaitingSearch
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
            ]
        ]


viewWorkspace : WorkingState.Model -> Html WorkingState.Msg
viewWorkspace model =
    SplitPane.view
        { direction = SplitPane.Horizontal
        , ratio = model.workbenchRatio
        , onResize = WorkingState.WorkbenchResized
        , first =
            EditorsView.view
                { elmCode = model.elmCode
                , onElmChange = WorkingState.ElmCodeChanged
                , htmlCode = model.htmlCode
                , onHtmlChange = WorkingState.HtmlCodeChanged
                , ratio = model.editorsRatio
                , onResize = WorkingState.EditorsResized
                }
        , second =
            Html.div [] []
        }


viewStyles model =
    Css.Foreign.global
        [ case model.user.settings.theme of
            Settings.Light ->
                Theme.lightStyles

            Settings.Dark ->
                Theme.darkStyles
        , Css.Foreign.selector ":root"
            [ property "--editor-font-size" model.user.settings.fontSize
            , property "--editor-font-family" model.user.settings.fontFamily
            ]
        ]
