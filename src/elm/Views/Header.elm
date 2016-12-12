module Views.Header exposing (..)

import Html exposing (Html, header, h1, button, div, text, span)
import Html.Events exposing (onClick)
import Views.Icons as Icons
import Views.Classes exposing (..)


viewButton : String -> Html msg
viewButton content =
    button [ class [ HeaderButton ] ]
        [ span [ class [ HeaderButtonIcon ] ]
            [ Icons.playOutline ]
        , span [] [ text "Update" ]
        ]


view : Html msg
view =
    header [ class [ Header ] ]
        [ div [ class [ HeaderLogo ] ]
            [ h1 [ class [ HeaderLogoText ] ] [ text "Ellie" ]
            ]
        , div [ class [ HeaderStatus ] ]
            [ span [ class [ HeaderStatusText ] ]
                [ text "Setting up session " ]
            , span [ class [ HeaderStatusEllipsis ] ]
                [ text "..." ]
            ]
        , viewButton "Compile"
        ]
