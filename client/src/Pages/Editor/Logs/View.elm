module Pages.Editor.Logs.View exposing (view)

import Data.Ellie.Log as Log exposing (Log)
import Ellie.Ui.Button as Button
import Ellie.Ui.TextInput as TextInput
import Html exposing (Html, div, text)
import Html.Attributes exposing (id)
import Pages.Editor.Logs.Model as Model exposing (Model)
import Pages.Editor.Logs.Styles as Styles
import Pages.Editor.Logs.Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ Styles.container ]
        [ viewLogs model
        , viewControls model
        ]


viewLogs : Model -> Html Msg
viewLogs model =
    div [ Styles.logs, id "pageEditorLogsLogs" ] <|
        (model.logs
            |> List.filter (.tag >> String.toLower >> String.contains (String.toLower model.search))
            |> List.reverse
            |> List.map viewLog
        )


viewLog : Log -> Html Msg
viewLog { tag, body } =
    div [ Styles.log ]
        [ div [ Styles.label ] [ text tag ]
        , div [ Styles.body ] [ text body ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    div [ Styles.controls ]
        [ div [ Styles.filterInput ]
            [ TextInput.view
                { placeholder = "Filter by label"
                , value = model.search
                , clearable = True
                , icon = Nothing
                , onChange = SearchChanged
                }
            ]
        , div [ Styles.clearButton ]
            [ Button.view
                { size = Button.Small
                , style = Button.Primary
                , icon = Nothing
                , label = "Clear Logs"
                , disabled = False
                , action = Button.click LogsCleared
                , attributes = []
                }
            ]
        ]
