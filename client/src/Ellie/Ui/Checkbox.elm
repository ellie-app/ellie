module Ellie.Ui.Checkbox exposing (..)

import Ellie.Ui.Checkbox.Styles as Styles
import Extra.Html.Attributes as Attributes
import Html exposing (Html, div, input, label, span)
import Html.Attributes exposing (attribute, checked, for, id, type_)
import Html.Events exposing (onCheck, onClick)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)


type alias Config msg =
    { onChange : Bool -> msg
    , checked : Bool
    , label : Html msg
    , id : String
    }


view : Config msg -> Html msg
view config =
    label
        [ Styles.container
        ]
        [ input
            [ type_ "checkbox"
            , checked config.checked
            , onCheck config.onChange
            , Styles.actualInput
            , id config.id
            ]
            []
        , svg
            [ Styles.svgCheckbox
            , attribute "data-checkbox" ""
            ]
            [ use
                [ Attributes.cond (xlinkHref "#checkbox-checked") config.checked
                , Attributes.cond (xlinkHref "#checkbox-unchecked") (not config.checked)
                ]
                []
            ]
        , div [ Styles.label, for config.id ] [ config.label ]
        ]
