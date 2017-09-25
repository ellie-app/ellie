module Ellie.Ui.TextArea exposing (view)

import Ellie.Ui.TextArea.Styles as Styles
import Html exposing (Html, button, div, textarea)
import Html.Attributes exposing (attribute, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (polygon, svg)
import Svg.Attributes exposing (points, viewBox)


type alias Config msg =
    { placeholder : String
    , value : String
    , onChange : String -> msg
    }


view : Config msg -> Html msg
view config =
    div [ Styles.container ]
        [ textarea
            [ placeholder config.placeholder
            , Styles.textarea
            , value config.value
            , onInput config.onChange
            ]
            []
        , svg
            [ viewBox "0 0 5 5"
            , Styles.resizer
            , attribute "data-resize" ""
            ]
            [ polygon [ points "5 0 5 5 0 5" ] [] ]
        , div [ Styles.underlineBottom ] []
        , div
            [ attribute "data-underline" ""
            , Styles.underlineTop
            ]
            []
        ]
