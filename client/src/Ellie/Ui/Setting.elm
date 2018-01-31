module Ellie.Ui.Setting exposing (Config, view)

import Colors
import Css exposing (..)
import Html.Styled exposing (Html, Attribute, div, label, text)
import Html.Styled.Attributes exposing (css)


type alias Config msg =
    { label : String
    , description : String
    , control : Html msg
    }


view : Config msg -> Html msg
view config =
    div []
        [ label [ labelStyles ]
            [ div [ titleStyles ] [ text config.label ]
            , div [ descriptionStyles ] [ text config.description ]
            ]
        , div [ controlStyles ] [ config.control ]
        ]


-- STYLES


titleStyles : Attribute msg
titleStyles =
    css
        [ fontSize (px 16)
        , color Colors.lightGray
        ]


labelStyles : Attribute msg
labelStyles =
    css [ display block ]


descriptionStyles : Attribute msg
descriptionStyles =
    css
        [ fontSize (px 12)
        , lineHeight (px 15)
        , color Colors.lightMediumGray
        , paddingTop (px 2)
        ]


controlStyles : Attribute msg
controlStyles =
    css [ paddingTop (px 12) ]
