module Ellie.Ui.Dropdown exposing (Config, view)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Html.Styled exposing (Html, Attribute, div, option, select, text)
import Html.Styled.Attributes exposing (attribute, disabled, selected, value)
import Html.Styled.Events exposing (on)
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List


type alias Config a msg =
    { items : List a
    , label : a -> String
    , selected : a -> Bool
    , onChange : a -> msg
    , disabled : Bool
    }


view : Config a msg -> Html msg
view config =
    div [ containerStyles ]
        [ select
            [ on "change" (Decode.map config.onChange (selectedValue config.items))
            , value <| (config.items |> List.findIndex config.selected |> Maybe.withDefault -1 |> toString)
            , selectStyles
            , disabled config.disabled
            ]
          <|
            List.indexedMap
                (\i a ->
                    option
                        [ value <| toString i
                        , selected <| config.selected a
                        ]
                        [ text <| config.label a ]
                )
                config.items
        , div
            [ underlineStyles
            , attribute "data-underline" ""
            ]
            []
        , div
            [ arrowStyles
            , attribute "data-arrow" ""
            ]
            [ Icon.view Icon.Chevron
            ]
        ]


selectedValue : List a -> Decoder a
selectedValue data =
    Decode.int
        |> Decode.at [ "target", "selectedIndex" ]
        |> Decode.andThen
            (\index ->
                data
                    |> List.drop index
                    |> List.head
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "index out of bounds")
            )


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , borderBottom3 (px 1) solid Colors.mediumGray
        ]


selectStyles : Attribute msg
selectStyles =
    css
        [ property "background" "none"
        , border zero
        , borderRadius zero
        , property "-webkit-appearance" "none"
        , width (pct 100)
        , fontFamily inherit
        , fontSize (px 15)
        , color Colors.lightGray
        , padding4 (px 8) (px 20) (px 8) zero
        , outline zero
        , cursor pointer
        , property "user-select" "none"
        , focus
            [ generalSiblings
                [ selector "[data-underline]"
                    [ transform <| scaleX 1
                    ]
                , selector "[data-arrow]"
                    [ color Colors.pink
                    ]
                ]
            ]
        , active
            [ generalSiblings
                [ selector "[data-arrow]"
                    [ transforms [ rotate (deg 180), translateY (pct 50) ]
                    ]
                ]
            ]
        , disabled
            [ color Colors.lightMediumGray
            , cursor notAllowed
            , generalSiblings
                [ selector "[data-arrow]"
                    [ color Colors.mediumGray ]
                ]
            ]
        ]


underlineStyles : Attribute msg
underlineStyles =
    css
        [ position absolute
        , bottom (px -1)
        , height (px 1)
        , width (pct 100)
        , backgroundColor Colors.pink
        , transform <| scaleX 0
        , property "transition" "transform 250ms"
        ]


arrowStyles : Attribute msg
arrowStyles =
    css
        [ position absolute
        , color Colors.lightMediumGray
        , width (px 12)
        , height (px 12)
        , right zero
        , top (pct 50)
        , transform <| translateY (pct -50)
        , property "pointer-events" "none"
        ]
