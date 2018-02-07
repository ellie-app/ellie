module Pages.Editor.View exposing (styles, view)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Constants as Constants
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.SplitPane as SplitPane
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)
import Pages.Editor.State.Actions as ActionsState
import Pages.Editor.State.App as AppState exposing (Model(..))
import Pages.Editor.State.Setup as SetupState
import Pages.Editor.Views.Editors as EditorsView
import Pages.Editor.Views.Packages as PackagesView
import Pages.Editor.Views.Setup as SetupView
import Pages.Editor.Views.Sidebar as SidebarView
import Pages.Editor.Views.Working as WorkingView


view : Model -> Html AppState.Msg
view model =
    case model of
        AppState.Initial _ _ ->
            Html.text ""

        AppState.Setup setupState ->
            case setupState of
                SetupState.Authenticating _ _ ->
                    SetupView.view SetupView.Authenticating

                SetupState.Attaching _ _ ->
                    SetupView.view SetupView.Attaching

                _ ->
                    SetupView.view SetupView.Loading

        AppState.Working workingState ->
            Html.map AppState.WorkingMsg <| WorkingView.view workingState

        AppState.Broken ->
            Html.text "It broke"



-- STYLES


styles : List Css.Foreign.Snippet
styles =
    [ Css.Foreign.html
        [ height (pct 100)
        , backgroundColor Colors.darkGray
        ]
    , Css.Foreign.body
        [ height (pct 100)
        , margin zero
        , fontFamilies [ Constants.sansFont ]
        , property "-webkit-font-smoothing" "antialiased"
        ]
    , Css.Foreign.everything
        [ boxSizing borderBox ]
    , Css.Foreign.button
        [ focus [ outline zero ] ]
    , Css.Foreign.input
        [ focus [ outline zero ] ]
    , Css.Foreign.id "elmEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 0)
        ]
    , Css.Foreign.id "htmlEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        ]
    ]


notificationsStyles : Attribute msg
notificationsStyles =
    css
        [ padding (px 16)
        ]
