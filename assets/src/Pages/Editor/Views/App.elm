module Pages.Editor.Views.App exposing (styles, title, view)

import Css exposing (..)
import Css.Global
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)
import Pages.Editor.State.App as AppState exposing (Model(..))
import Pages.Editor.State.Setup as SetupState
import Pages.Editor.Views.Setup as SetupView
import Pages.Editor.Views.Working as WorkingView


view : Model -> Html AppState.Msg
view model =
    case model of
        AppState.Initial _ _ ->
            Html.text ""

        AppState.Setup setupModel ->
            case setupModel.state of
                SetupState.Authenticating _ ->
                    SetupView.view SetupView.Authenticating

                SetupState.AcceptingTerms state ->
                    { termsVersion = state.latestTerms
                    , onAccept = SetupState.UserAcceptedTerms
                    }
                        |> SetupView.AcceptingTerms
                        |> SetupView.view
                        |> Html.map AppState.SetupMsg

                SetupState.Attaching _ ->
                    SetupView.view SetupView.Attaching

                SetupState.Failure _ ->
                    SetupView.view SetupView.Failure

                _ ->
                    SetupView.view SetupView.Loading

        AppState.Working workingState ->
            Html.map AppState.WorkingMsg <| WorkingView.view workingState

        AppState.Broken ->
            Html.text "It broke"


title : Model -> String
title model =
    case model of
        AppState.Working workingState ->
            case workingState.projectName of
                "" ->
                    "Ellie - Untitled"

                name ->
                    "Ellie - " ++ workingState.projectName

        _ ->
            "Ellie"



-- STYLES


styles : List Css.Global.Snippet
styles =
    [ Css.Global.html
        [ height (pct 100)
        , backgroundColor Theme.secondaryBackground
        ]
    , Css.Global.body
        [ height (pct 100)
        , margin zero
        , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif" ]
        , property "-webkit-font-smoothing" "antialiased"
        ]
    , Css.Global.everything
        [ boxSizing borderBox ]
    , Css.Global.button
        [ focus [ outline zero ] ]
    , Css.Global.input
        [ focus [ outline zero ] ]
    , Css.Global.id "elmEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 0)
        ]
    , Css.Global.id "htmlEditor"
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        ]
    , Theme.darkStyles
    ]


notificationsStyles : Attribute msg
notificationsStyles =
    css
        [ padding (px 16)
        ]
