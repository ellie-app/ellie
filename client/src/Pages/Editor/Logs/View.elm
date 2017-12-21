module Pages.Editor.Logs.View exposing (view)

import BoundedDeque
import Data.Ellie.Log as Log exposing (Log)
import Ellie.Ui.Button as Button
import Ellie.Ui.TextInput as TextInput
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (on)
import Json.Decode as Decode
import Pages.Editor.Logs.Model as Model exposing (Model)
import Pages.Editor.Logs.Styles as Styles
import Pages.Editor.Logs.Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ Styles.container ]
        [ viewLogs model
        , viewControls model
        ]


onScrolledToBottom : Attribute Msg
onScrolledToBottom =
    on "scroll" <|
        Decode.map3
            (\scrollTop clientHeight scrollHeight ->
                UpdateScrollState <| scrollTop >= scrollHeight - clientHeight
            )
            (Decode.at [ "target", "scrollTop" ] Decode.float)
            (Decode.at [ "target", "clientHeight" ] Decode.float)
            (Decode.at [ "target", "scrollHeight" ] Decode.float)


viewLogs : Model -> Html Msg
viewLogs model =
    div
        [ Styles.logs
        , id "pagesEditorLogsLogs"
        , onScrolledToBottom
        ]
        (model.logs
            |> BoundedDeque.filter (.tag >> String.toLower >> String.contains (String.toLower model.search))
            |> BoundedDeque.map viewLog
            |> BoundedDeque.toList
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
