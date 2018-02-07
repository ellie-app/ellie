module Ellie.Ui.Settings exposing (..)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)


type alias Config =
    { id : String
    }


view : Config -> Html msg
view config =
    Html.div [ css [ position relative ] ]
        [ Html.label
            [ css
                [ width (px 16)
                , height (px 16)
                , color Colors.lightMediumGray
                , display block
                ]
            ]
            [ Icon.view Icon.Settings ]
        ]
