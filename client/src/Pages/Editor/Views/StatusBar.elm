module Pages.Editor.Views.StatusBar exposing (..)

import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


type alias Config =
    { connected : Bool
    }


view : Config -> Html msg
view config =
    Html.node "ellie-status-bar"
        [ css
            [ width (pct 100)
            , height (px 32)
            , borderTop3 (px 2) solid Theme.staticBorder
            , flexShrink zero
            , justifyContent flexEnd
            , alignItems center
            , displayFlex
            , backgroundColor Theme.primaryBackground
            , padding2 zero (px 12)
            ]
        ]
        [ Html.div
            [ css
                [ width (px 20)
                , height (px 20)
                , color <|
                    if config.connected then
                        Theme.connectionStatusConnected
                    else
                        Theme.connectionStatusDisconnected
                ]
            ]
            [ Icon.view Icon.Socket
            ]
        ]
