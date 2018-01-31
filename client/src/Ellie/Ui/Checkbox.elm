module Ellie.Ui.Checkbox exposing (Config, view)

import Colors
import Css exposing (..)
import Css.Foreign
import Extra.Html.Attributes as Attributes
import Html.Styled exposing (Html, Attribute, div, input, label, span)
import Html.Styled.Attributes as Attributes exposing (css, attribute, for, id, type_)
import Html.Styled.Events exposing (onCheck, onClick)
import Svg.Styled exposing (svg, use)
import Svg.Styled.Attributes exposing (xlinkHref)


type alias Config msg =
    { onChange : Bool -> msg
    , checked : Bool
    , label : Html msg
    , id : String
    }


view : Config msg -> Html msg
view config =
    label [ containerStyles ]
        [ input
            [ type_ "checkbox"
            , Attributes.checked config.checked
            , onCheck config.onChange
            , actualInputStyles
            , id config.id
            ]
            []
        , svg
            [ svgCheckboxStyles
            , attribute "data-checkbox" ""
            ]
            [ use
                [ Attributes.cond (xlinkHref "#checkbox-checked") config.checked
                , Attributes.cond (xlinkHref "#checkbox-unchecked") (not config.checked)
                ]
                []
            ]
        , div
            [ labelStyles
            , for config.id
            ]
            [ config.label ]
        ]


-- STYLES


actualInputStyles : Attribute msg
actualInputStyles =
    css [ display none ]


containerStyles : Attribute msg
containerStyles =
    css
        [ displayFlex
        , cursor pointer
        , alignItems center
        , hover
            [ Css.Foreign.descendants
                [ Css.Foreign.selector "[data-checkbox]"
                    [ fill Colors.lightMediumGray
                    ]
                ]
            ]
        , active
            [ Css.Foreign.descendants
                [ Css.Foreign.selector "[data-checkbox]"
                    [ fill Colors.mediumGray ]
                ]
            ]
        ]


labelStyles : Attribute msg
labelStyles =
    css
        [ cursor pointer
        , marginLeft (px 8)
        ]


svgCheckboxStyles : Attribute msg
svgCheckboxStyles =
    css
        [ width (px 16)
        , height (px 16)
        , fill Colors.mediumGray
        ]
