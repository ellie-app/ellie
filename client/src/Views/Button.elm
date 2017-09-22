module Views.Button exposing (Config, view)

import Extra.Html.Attributes as Attributes exposing (style)
import Extra.String as String
import Html exposing (Html, button, span, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Views.Button.Styles as Styles


type alias Config msg =
    { onClick : msg
    , disabled : Bool
    , label : String
    , width : Maybe Int
    }


view : Config msg -> Html msg
view config =
    button
        [ Styles.button
        , disabled config.disabled
        , onClick config.onClick
        , config.width
            |> Maybe.map (String.fromInt >> flip (++) "px" >> style "width")
            |> Maybe.withDefault Attributes.none
        ]
        [ text config.label ]
