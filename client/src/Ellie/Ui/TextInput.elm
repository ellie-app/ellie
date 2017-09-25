module Ellie.Ui.TextInput exposing (view)

import Ellie.Ui.Icon as Icon
import Ellie.Ui.TextInput.Styles as Styles
import Extra.Html as Html
import Html exposing (Html, button, div, input)
import Html.Attributes exposing (attribute, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Config msg =
    { placeholder : String
    , value : String
    , clearable : Bool
    , icon : Maybe Icon.Icon
    , onChange : String -> msg
    }


view : Config msg -> Html msg
view config =
    div [ Styles.container ]
        [ config.icon
            |> Maybe.map (Icon.view >> List.singleton >> div [ Styles.icon ])
            |> Html.maybe
        , input
            [ type_ "text"
            , placeholder config.placeholder
            , Styles.input
            , value config.value
            , onInput config.onChange
            ]
            []
        , div
            [ attribute "data-underline" ""
            , Styles.underline
            ]
            []
        , Html.viewIf (config.value /= "" && config.clearable) <|
            button
                [ Styles.clearButton
                , onClick <| config.onChange ""
                ]
                [ Icon.view Icon.Close ]
        ]
