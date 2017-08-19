module Views.Button.View exposing (Config, CssClass(..), loading, namespace, view)

import Extra.Html.Attributes as Attributes exposing (style)
import Extra.String as String
import Html exposing (Html, button, span, text)
import Html.Attributes exposing (disabled)
import Html.CssHelpers
import Html.Events exposing (onClick)


type alias Config msg =
    { onClick : msg
    , disabled : Bool
    , label : String
    , width : Maybe Int
    }


view : Config msg -> Html msg
view config =
    button
        [ class [ Button ]
        , disabled config.disabled
        , onClick config.onClick
        , config.width
            |> Maybe.map (String.fromInt >> flip (++) "px" >> style "width")
            |> Maybe.withDefault Attributes.none
        ]
        [ text config.label ]


loading :
    { width : Maybe Int }
    -> Html msg
loading config =
    button
        [ class [ Button, Loading ]
        , disabled True
        , config.width
            |> Maybe.map (String.fromInt >> flip (++) "px" >> style "width")
            |> Maybe.withDefault Attributes.none
        ]
        [ span [ class [ LoadingFiller ] ] []
        ]


{ class } =
    Html.CssHelpers.withNamespace namespace


namespace : String
namespace =
    "Views-Button-"


type CssClass
    = Button
    | Loading
    | LoadingFiller
