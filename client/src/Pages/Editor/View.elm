module Pages.Editor.View exposing (styles, view)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Constants as Constants
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.SplitPane as SplitPane
import Html.Styled as Html exposing (Attribute, Html, button, div, header, iframe, main_, span, text)
import Html.Styled.Attributes exposing (css)
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Update as Update exposing (Msg(..))
import Pages.Editor.Views.Loading as Loading


type ViewModel
    = Loading Loading.Stage


viewModel : Model -> ViewModel
viewModel model =
    case ( model.workspace, model.token ) of
        ( _, Nothing ) ->
            Loading Loading.Authenticating

        _ ->
            Loading Loading.CreatingWorkspace


view : Model -> Html Msg
view model =
    SplitPane.view
        (CodeEditor.view [ CodeEditor.value "hi" ])
        (CodeEditor.view [ CodeEditor.value "hello" ])



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
