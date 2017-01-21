module Components.Splash.View
    exposing
        ( view
        )

import Html exposing (Html, div, h1, p, text)
import Components.Splash.Classes as Classes exposing (Classes(..), class)


view : Html msg
view =
    div [ class [ Container ] ]
        [ h1 [ class [ MainText ] ]
            [ text "Hi! I’m Ellie." ]
        , p [ class [ SubText ] ]
            [ text "I'm setting up your session." ]
        , p [ class [ SubText ] ]
            [ text "It should only take a few more seconds." ]
        ]
