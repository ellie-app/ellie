module Ellie.Ui.Checkbox exposing (Config, view)

import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Theme as Theme
import Extra.Html.Attributes as Attributes
import Html.Styled exposing (Attribute, Html, div, input, label, span)
import Html.Styled.Attributes as Attributes exposing (attribute, css, for, id, type_)
import Html.Styled.Events exposing (onCheck, onClick)
import Svg.Styled as Svg exposing (svg)
import Svg.Styled.Attributes as Svg


type alias Config msg =
    { onChange : Bool -> msg
    , checked : Bool
    , label : Html msg
    , id : String
    }


view : Config msg -> Html msg
view config =
    label
        [ for config.id
        , css
            [ displayFlex
            , cursor pointer
            ]
        ]
        [ input
            [ type_ "checkbox"
            , Attributes.checked config.checked
            , onCheck config.onChange
            , id config.id
            , css
                [ opacity (num 0)
                , position absolute
                , left (px -999)
                , checked
                    [ Css.Foreign.adjacentSiblings
                        [ Css.Foreign.div
                            [ Css.Foreign.descendants
                                [ Css.Foreign.selector "[data-ui-checkbox-check]" [ display block ]
                                ]
                            ]
                        ]
                    ]
                , focus
                    [ Css.Foreign.adjacentSiblings
                        [ Css.Foreign.div
                            [ borderColor Theme.accent
                            ]
                        ]
                    ]
                , active
                    [ Css.Foreign.adjacentSiblings
                        [ Css.Foreign.div
                            [ borderColor Theme.accent
                            ]
                        ]
                    ]
                ]
            ]
            []
        , div
            [ css
                [ backgroundColor Theme.secondaryBackground
                , border3 (px 1) solid Theme.controlBorder
                , color Theme.secondaryForeground
                , width (px 20)
                , height (px 20)
                , displayFlex
                , padding2 (px 4) (px 2)
                ]
            ]
            [ checkbox ]
        , div
            [ css
                [ cursor pointer
                , marginLeft (px 8)
                ]
            ]
            [ config.label ]
        ]


checkbox : Html msg
checkbox =
    svg
        [ Svg.viewBox "0 0 14 11"
        , attribute "data-ui-checkbox-check" ""
        , Svg.css
            [ width (px 14)
            , height (px 11)
            , fill currentColor
            , display none
            ]
        ]
        [ Svg.polygon
            [ Svg.points "2.84217094e-14 5.71148221 1.39987812 4.23083407 4.99997991 8.03865414 12.6001219 2.13162821e-14 14 1.48069772 4.99997991 11"
            ]
            []
        ]
