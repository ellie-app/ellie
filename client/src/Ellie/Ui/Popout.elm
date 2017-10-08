module Ellie.Ui.Popout exposing (..)

import Ellie.Ui.Popout.Styles as Styles
import Extra.Html as Html
import Extra.Html.Attributes as Attributes
import Html exposing (..)
import Html.Events exposing (onClick)


type alias Config msg =
    { tooltip : Html msg
    , content : Html msg
    , open : Bool
    , onToggle : Bool -> msg
    , disabled : Bool
    }


view : Config msg -> Html msg
view config =
    div [ Styles.container ]
        [ Html.viewIf (config.open && not config.disabled) <|
            div
                [ Styles.overlay
                , onClick <| config.onToggle False
                ]
                []
        , div
            [ Styles.content
            , onClick <| config.onToggle (not config.open)
            ]
            [ config.content ]
        , div
            [ Attributes.cond Styles.open <| config.open && not config.disabled
            , Styles.tooltip
            ]
            [ config.tooltip ]
        ]
