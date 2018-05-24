module Ellie.Ui.Switch exposing (Config, view)

import Colors
import Css exposing (..)
import Css.Foreign
import Extra.Html.Attributes as Attributes
import Html.Styled as Html exposing (Attribute, Html, div, input, label, span)
import Html.Styled.Attributes as Attributes exposing (attribute, css, for, id, type_)
import Html.Styled.Events exposing (onCheck, onClick, onWithOptions)
import Json.Decode as Decode
import Svg.Styled as Svg exposing (svg)
import Svg.Styled.Attributes as Svg


type alias Config msg =
    { onChange : Bool -> msg
    , on : Bool
    , onLabel : String
    , offLabel : String
    , id : String
    }


view : Config msg -> Html msg
view config =
    label
        [ css
            [ display inlineFlex
            , cursor pointer
            ]
        ]
        [ input
            [ type_ "checkbox"
            , Attributes.checked config.on
            , id config.id
            , css
                [ opacity (num 0)
                , position absolute
                , left (px -999)
                , checked
                    [ Css.Foreign.generalSiblings
                        [ Css.Foreign.selector "[data-ui-switch-toggler]"
                            [ Css.Foreign.descendants
                                [ Css.Foreign.selector "[data-ui-switch-toggler-circle]" [ property "cx" "28" ]
                                ]
                            ]
                        ]
                    ]
                , focus [ activeFocus ]
                , active [ activeFocus ]
                ]
            ]
            []
        , div
            [ labelTextStyles True
            , onWithOptions
                "click"
                { stopPropagation = True, preventDefault = False }
                (Decode.succeed (config.onChange False))
            , attribute "data-ui-switch-left-label" ""
            ]
            [ Html.text config.offLabel ]
        , div
            [ onWithOptions
                "click"
                { stopPropagation = True, preventDefault = False }
                (Decode.succeed (config.onChange (not config.on)))
            , attribute "data-ui-switch-toggler" ""
            ]
            [ switch ]
        , div
            [ labelTextStyles False
            , onWithOptions
                "click"
                { stopPropagation = True, preventDefault = False }
                (Decode.succeed (config.onChange True))
            , attribute "data-ui-switch-right-label" ""
            ]
            [ Html.text config.onLabel ]
        ]


labelTextStyles : Bool -> Attribute msg
labelTextStyles leftSide =
    css
        [ fontSize (px 16)
        , lineHeight (num 1)
        , fontWeight bold
        , color Colors.lightGray
        , property "user-select" "none"
        , if leftSide then
            marginRight (px 8)
          else
            marginLeft (px 8)
        ]


activeFocus =
    Css.Foreign.generalSiblings
        [ Css.Foreign.selector "[data-ui-switch-toggler]"
            [ Css.Foreign.descendants
                [ Css.Foreign.selector "[data-ui-switch-toggler-outline]" [ property "stroke" (.value Colors.pink) ]
                ]
            ]
        ]


switch : Html msg
switch =
    svg
        [ Svg.viewBox "0 0 38 20"
        , Svg.css
            [ width (px 38)
            , height (px 20)
            ]
        ]
        [ Svg.rect [ Svg.x "0", Svg.y "0", Svg.width "38", Svg.height "20", Svg.rx "10" ] []
        , Svg.rect
            [ Svg.x "0.5"
            , Svg.y "0.5"
            , Svg.width "37"
            , Svg.height "19"
            , Svg.rx "9.5"
            , Svg.strokeWidth "1"
            , Svg.stroke (.value Colors.lightMediumGray)
            , attribute "data-ui-switch-toggler-outline" ""
            ]
            []
        , Svg.circle
            [ Svg.cx "10"
            , Svg.cy "10"
            , Svg.r "8"
            , Svg.css [ fill Colors.lightMediumGray ]
            , attribute "data-ui-switch-toggler-circle" ""
            ]
            []
        ]
