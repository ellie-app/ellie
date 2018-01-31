module Ellie.Ui.TextInput exposing (view)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Ui.Icon as Icon
import Extra.Html as Html
import Html.Styled exposing (Html, Attribute, button, div, input)
import Html.Styled.Attributes exposing (css, attribute, placeholder, type_, value)
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
    div [ containerStyles ]
        [ config.icon
            |> Maybe.map (Icon.view >> List.singleton >> div [ iconStyles ])
            |> Html.maybe
        , input
            [ type_ "text"
            , placeholder config.placeholder
            , inputStyles
            , value config.value
            , onInput config.onChange
            ]
            []
        , div
            [ attribute "data-underline" ""
            , underlineStyles
            ]
            []
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
        [ width (px 15)
        , height (px 15)
        , color Colors.lightMediumGray
        , flexShrink (int 0)
        , marginRight (px 8)
        , marginTop (px 2)
        ]


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , borderBottom3 (px 1) solid Colors.lightMediumGray
        , displayFlex
        , alignItems flexStart
        ]


inputStyles : Attribute msg
inputStyles =
    css
        [ property "background" "none"
        , border zero
        , display block
        , width (pct 100)
        , fontSize (px 15)
        , lineHeight (num 1)
        , color Colors.lightGray
        , fontFamily monospace
        , padding3 zero zero (px 8)
        , focus
            [ Css.Foreign.adjacentSiblings
                [ Css.Foreign.selector "[data-underline]"
                    [ transform <| scaleX 1 ]
                ]
            ]
        , pseudoElement "-ms-input-placeholder" [ color Colors.mediumGray ]
        , pseudoElement "-webkit-input-placeholder" [ color Colors.mediumGray ]
        , pseudoElement "-moz-placeholder" [ color Colors.mediumGray ]
        ]


underlineStyles : Attribute msg
underlineStyles =
    css
        [ backgroundColor Colors.pink
        , height (px 1)
        , position absolute
        , bottom (px -1)
        , width (pct 100)
        , transform <| scaleX 0
        , property "transition" "transform 250ms"
        , left zero
        ]


clearButtonStyles : Attribute msg
clearButtonStyles =
    css
        [ property "background" "none"
        , border zero
        , color Colors.mediumGray
        , width (px 12)
        , height (px 12)
        , right zero
        , top (px 11)
        , padding zero
        , cursor pointer
        , property "transition" "color 250ms"
        , hover
            [ color Colors.lightMediumGray ]
        , marginLeft (px 8)
        , flexShrink (int 0)
        , marginTop (px 2)
        ]
