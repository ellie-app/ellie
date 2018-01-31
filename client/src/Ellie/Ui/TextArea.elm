module Ellie.Ui.TextArea exposing (view)

import Colors
import Css exposing (..)
import Css.Foreign
import Html.Styled exposing (Html, Attribute, button, div, textarea)
import Html.Styled.Attributes exposing (css, attribute, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Svg.Styled exposing (polygon, svg)
import Svg.Styled.Attributes as Svg exposing (points)


type alias Config msg =
    { placeholder : String
    , value : String
    , onChange : String -> msg
    }


view : Config msg -> Html msg
view config =
    div [ containerStyles ]
        [ textarea
            [ placeholder config.placeholder
            , textareaStyles
            , value config.value
            , onInput config.onChange
            ]
            []
        , svg
            [ Svg.viewBox "0 0 5 5"
            , resizerStyles
            , attribute "data-resize" ""
            ]
            [ polygon [ points "5 0 5 5 0 5" ] [] ]
        , div [ underlineBottomStyles ] []
        , div
            [ attribute "data-underline" ""
            , underlineTopStyles
            ]
            []
        ]


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , overflow hidden
        ]


textareaStyles : Attribute msg
textareaStyles =
    css
        [ property "background" "none"
        , border zero
        , display block
        , width (pct 100)
        , fontSize (px 15)
        , lineHeight (num 1)
        , color Colors.lightGray
        , fontFamily monospace
        , padding zero
        , paddingBottom (px 9)
        , marginBottom (px -8)
        , resize vertical
        , minHeight (px 56)
        , outline zero
        , position relative
        , zIndex (int 1)
        , focus
            [ Css.Foreign.generalSiblings
                [ Css.Foreign.selector "[data-underline]"
                    [ transform <| scaleX 1 ]
                , Css.Foreign.selector "[data-resize]"
                    [ fill Colors.pink
                    , property "transition-delay" "200ms"
                    ]
                ]
            ]
        , pseudoElement "-ms-input-placeholder" [ color Colors.mediumGray ]
        , pseudoElement "-webkit-input-placeholder" [ color Colors.mediumGray ]
        , pseudoElement "-moz-placeholder" [ color Colors.mediumGray ]
        ]


underlineBaseStyles : Style
underlineBaseStyles =
    batch
        [ height (px 1)
        , position absolute
        , bottom zero
        , width (pct 100)
        ]


underlineBottomStyles : Attribute msg
underlineBottomStyles =
    css
        [ underlineBaseStyles
        , backgroundColor Colors.lightMediumGray
        ]


underlineTopStyles : Attribute msg
underlineTopStyles =
    css
        [ underlineBaseStyles
        , transform <| scaleX 0
        , property "transition" "transform 250ms"
        , backgroundColor Colors.pink
        ]


resizerStyles : Attribute msg
resizerStyles =
    Svg.css
        [ width (px 7)
        , height (px 7)
        , position absolute
        , bottom zero
        , right zero
        , zIndex (int 0)
        , fill Colors.lightMediumGray
        , property "transition" "fill 50ms"
        , cursor grab
        ]
