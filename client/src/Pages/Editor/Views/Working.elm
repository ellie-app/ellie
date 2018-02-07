module Pages.Editor.Views.Working exposing (view)

import Css exposing (..)
import Ellie.Ui.SplitPane as SplitPane
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)
import Pages.Editor.State.Actions as ActionsState
import Pages.Editor.State.Working as WorkingState
import Pages.Editor.Views.Editors as EditorsView
import Pages.Editor.Views.Packages as PackagesView
import Pages.Editor.Views.Sidebar as SidebarView


view : WorkingState.Model -> Html WorkingState.Msg
view model =
    Html.main_
        [ css [ displayFlex ]
        ]
        [ SidebarView.view
        , case model.actions of
            ActionsState.Hidden ->
                viewWorkspace model

            ActionsState.Packages packagesModel ->
                SplitPane.view
                    { direction = SplitPane.Horizontal
                    , defaultRatio = 0.3
                    , first =
                        Html.map WorkingState.ActionsMsg <|
                            PackagesView.view
                                { query = packagesModel.query
                                , onSearch = ActionsState.UserTypedInPackageSearch
                                , installedPackages = model.packages
                                , searchedPackages = packagesModel.searchedPackages
                                }
                    , second =
                        viewWorkspace model
                    }

            _ ->
                Html.text ""
        ]


viewWorkspace : WorkingState.Model -> Html WorkingState.Msg
viewWorkspace model =
    SplitPane.view
        { direction = SplitPane.Horizontal
        , defaultRatio = 0.5
        , first =
            EditorsView.view
                { elmCode = model.elmCode
                , onElmChange = WorkingState.ElmCodeChanged
                , htmlCode = model.htmlCode
                , onHtmlChange = WorkingState.HtmlCodeChanged
                }
        , second =
            Html.div [] []
        }
