module Views.Modal exposing (ViewModel, view)

import Html exposing (Html, div)
import Html.Events exposing (onClick)
import Views.Modal.Styles as Styles


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
                    [ Styles.backdrop
                    , onClick msg
                    ]

                Nothing ->
                    [ Styles.backdrop ]
    in
    div [ Styles.container ]
        [ div backdropAttrs []
        , div [ Styles.content ] content
        ]
