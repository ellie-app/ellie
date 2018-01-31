module Ellie.Ui.Popout exposing (..)

import Colors
import Css exposing (..)
import Extra.Html as Html
import Extra.Html.Attributes as Attributes
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


type alias Config msg =
    { tooltip : Html msg
    , content : Html msg
    , open : Bool
    , onToggle : Bool -> msg
    , disabled : Bool
    }


view : Config msg -> Html msg
view config =
    div [ containerStyles ]
        [ Html.viewIf (config.open && not config.disabled) <|
            div
                [ overlayStyles
                , onClick <| config.onToggle False
                ]
                []
        , div
            [ contentStyles
            , onClick <| config.onToggle (not config.open)
            ]
            [ config.content ]
        , div
            [ tooltipStyles <| config.open && not config.disabled
            ]
            [ config.tooltip ]
        ]


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        ]


contentStyles : Attribute msg
contentStyles =
    css
        [ outline zero
        , position relative
        , zIndex (int 2)
        ]


tooltipStyles : Bool -> Attribute msg
tooltipStyles isOpen =
    css
        [ position absolute
        , display none
        , zIndex (int 2)
        , backgroundColor Colors.darkGray
        , padding2 (px 12) (px 8)
        , Colors.boxShadow |> .popout
        , top (pct 100)
        , marginTop (px 8)
        , borderLeft3 (px 1) solid Colors.pink
        , if isOpen then
            batch [ display block |> important ]
          else
            batch []
        ]


overlayStyles : Attribute msg
overlayStyles =
    css
        [ position fixed
        , width (pct 100)
        , height (pct 100)
        , left zero
        , top zero
        , zIndex (int 1)
        ]
