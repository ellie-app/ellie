module Ellie.Ui.TextInput exposing (view)

import Css exposing (..)
import Css.Global
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Extra.Html as Html
import Extra.Html.Attributes as Attributes
import Extra.Maybe as Maybe
import Html.Styled exposing (Attribute, Html, button, div, input)
import Html.Styled.Attributes exposing (attribute, autofocus, css, placeholder, tabindex, type_, value)
import Html.Styled.Events as Events exposing (onClick, onInput)
import Json.Decode as Decode


type alias Config msg =
    { placeholder : String
    , value : String
    , clearable : Bool
    , icon : Maybe Icon.Icon
    , onChange : String -> msg
    , autofocus : Bool
    }


clearOnEscape : (String -> msg) -> Attribute msg
clearOnEscape onChange =
    Events.onWithOptions "keydown"
        { preventDefault = True, stopPropagation = True }
        (Events.keyCode
            |> Decode.andThen
                (\keycode ->
                    if keycode == 27 then
                        Decode.succeed <| onChange ""

                    else
                        Decode.fail ""
                )
        )


view : Config msg -> Html msg
view config =
    div
        [ containerStyles ]
        [ input
            [ type_ "text"
            , placeholder config.placeholder
            , value config.value
            , onInput config.onChange
            , autofocus config.autofocus
            , if config.clearable then
                clearOnEscape config.onChange

              else
                Attributes.none
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
                , tabindex -1
                ]
                [ Icon.view Icon.Close ]
        ]



-- STYLES


iconStyles : Attribute msg
iconStyles =
    css
        [ width (px 30)
        , height (px 22)
        , color Theme.secondaryForeground
        , padding2 (px 3) (px 7)
        , position absolute
        , left (px 1)
        , top (px 5)
        , borderRight3 (px 1) solid Theme.controlBorder
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
        , fontFamily inherit
        , border zero
        , padding2 zero (px 8)
        , display block
        , border3 (px 1) solid Theme.controlBorder
        , width (pct 100)
        , fontSize (px 15)
        , lineHeight (num 1)
        , color Theme.primaryForeground
        , height (pct 100)
        , focus
            [ border3 (px 1) solid Theme.accent
            , Css.Global.adjacentSiblings
                [ Css.Global.div
                    [ borderRightColor Theme.accent
                    ]
                ]
            ]
        , pseudoElement "-ms-input-placeholder" [ color Theme.secondaryForeground ]
        , pseudoElement "-webkit-input-placeholder" [ color Theme.secondaryForeground ]
        , pseudoElement "-moz-placeholder" [ color Theme.secondaryForeground ]
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
        , color Theme.secondaryForeground
        , width (px 32)
        , height (px 32)
        , padding2 (px 10) (px 8)
        , cursor pointer
        , property "transition" "color 250ms"
        , hover [ color Theme.primaryForeground ]
        , position absolute
        , right zero
        , top zero
        ]
