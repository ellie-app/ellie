module Ellie.Ui.TextInput exposing (view)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Icon as Icon
import Extra.Html as Html
import Extra.Maybe as Maybe
import Html.Styled exposing (Attribute, Html, button, div, input)
import Html.Styled.Attributes exposing (attribute, css, placeholder, type_, value)
import Html.Styled.Events exposing (onClick, onInput)


type alias Config msg =
    { placeholder : String
    , value : String
    , clearable : Bool
    , icon : Maybe Icon.Icon
    , onChange : String -> msg
    }


view : Config msg -> Html msg
view config =
    div
        [ containerStyles ]
        [ input
            [ type_ "text"
            , placeholder config.placeholder
            , value config.value
            , onInput config.onChange
            , inputStyles
                (Maybe.isJust config.icon)
                (config.value /= "" && config.clearable)
            ]
            []
        , config.icon
            |> Maybe.map (Icon.view >> List.singleton >> div [ iconStyles ])
            |> Html.maybe
        , Html.viewIf (config.value /= "" && config.clearable) <|
            button
                [ clearButtonStyles
                , onClick <| config.onChange ""
                ]
                [ Icon.view Icon.Close ]
        ]



-- STYLES


iconStyles : Attribute msg
iconStyles =
    css
        [ width (px 30)
        , height (px 22)
        , color Colors.lightMediumGray
        , padding2 (px 3) (px 7)
        , position absolute
        , left (px 1)
        , top (px 5)
        , borderRight3 (px 1) solid Colors.mediumGray
        ]


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , height (px 32)
        , display block
        ]


inputStyles : Bool -> Bool -> Attribute msg
inputStyles hasIcon hasClearButton =
    css
        [ property "background" "none"
        , border zero
        , padding2 zero (px 8)
        , display block
        , border3 (px 1) solid Colors.mediumGray
        , width (pct 100)
        , fontSize (px 15)
        , lineHeight (num 1)
        , color Colors.lightGray
        , height (pct 100)
        , focus
            [ border3 (px 1) solid Colors.pink
            , Css.Foreign.adjacentSiblings
                [ Css.Foreign.div
                    [ borderRightColor Colors.pink
                    ]
                ]
            ]
        , pseudoElement "-ms-input-placeholder" [ color Colors.mediumGray ]
        , pseudoElement "-webkit-input-placeholder" [ color Colors.mediumGray ]
        , pseudoElement "-moz-placeholder" [ color Colors.mediumGray ]
        , if hasIcon then
            batch [ paddingLeft (px 40) ]
          else
            batch []
        , if hasClearButton then
            batch [ paddingRight (px 28) ]
          else
            batch []
        ]


clearButtonStyles : Attribute msg
clearButtonStyles =
    css
        [ property "background" "none"
        , border zero
        , color Colors.mediumGray
        , width (px 32)
        , height (px 32)
        , padding2 (px 10) (px 8)
        , cursor pointer
        , property "transition" "color 250ms"
        , hover [ color Colors.lightMediumGray ]
        , position absolute
        , right zero
        , top zero
        ]
