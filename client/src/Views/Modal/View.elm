module Views.Modal.View exposing (ViewModel, view)

import Html exposing (Html, div)
import Html.Events exposing (onClick)
import Views.Modal.Classes exposing (..)


type alias ViewModel msg =
    { onClose : msg
    , content : List (Html msg)
    }


view : ViewModel msg -> Html msg
view { onClose, content } =
    div [ class [ Container ] ]
        [ div
            [ class [ Backdrop ]
            , onClick onClose
            ]
            []
        , div [ class [ Content ] ] content
        ]
