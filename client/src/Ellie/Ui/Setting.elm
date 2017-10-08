module Ellie.Ui.Setting exposing (Config, view)

import Ellie.Ui.Setting.Styles as Styles
import Html exposing (Html, div, label, text)


type alias Config msg =
    { label : String
    , description : String
    , control : Html msg
    }


view : Config msg -> Html msg
view config =
    div []
        [ label [ Styles.label ]
            [ div [ Styles.title ] [ text config.label ]
            , div [ Styles.description ] [ text config.description ]
            ]
        , div [ Styles.control ] [ config.control ]
        ]
