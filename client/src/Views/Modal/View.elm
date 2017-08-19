module Views.Modal.View exposing (ViewModel, view)

import Html exposing (Html, div)
import Html.Events exposing (onClick)
import Views.Modal.Classes exposing (..)


type alias ViewModel msg =
    { onClose : Maybe msg
    , content : List (Html msg)
    }


view : ViewModel msg -> Html msg
view { onClose, content } =
    let
        backdropAttrs =
            case onClose of
                Just msg ->
                    [ class [ Backdrop ]
                    , onClick msg
                    ]

                Nothing ->
                    [ class [ Backdrop ] ]
    in
    div [ class [ Container ] ]
        [ div backdropAttrs []
        , div [ class [ Content ] ] content
        ]
