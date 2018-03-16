module Pages.Editor.Views.StatusBar exposing (..)

import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Pages.Editor.Types.Notification as Notification exposing (Notification)
import Pages.Editor.Views.Notifications as Notifications


type alias Config =
    { connected : Bool
    , compileStatus : String
    , notificationCount : Int
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
            , position relative
            ]
        ]
        [ Html.div
            [ css
                [ color Theme.primaryForeground
                , fontSize (px 16)
                , paddingRight (px 16)
                ]
            ]
            [ Html.text config.compileStatus
            ]
        , Html.div
            [ css
                [ width (px 20)
                , height (px 20)
                , color <|
                    if config.connected then
                        Theme.connectionStatusConnected
                    else
                        Theme.connectionStatusDisconnected
                ]
            , Attributes.title "Server connection status"
            ]
            [ Icon.view Icon.Socket
            ]
        , Html.div
            [ css
                [ displayFlex
                , alignItems center
                , paddingLeft (px 16)
                , color Theme.primaryForeground
                ]
            , Attributes.title <|
                toString config.notificationCount
                    ++ " notification"
                    ++ (if config.notificationCount == 1 then
                            ""
                        else
                            "s"
                       )
            ]
            [ Html.div
                [ css
                    [ width (px 20)
                    , height (px 20)
                    , color Theme.primaryForeground
                    ]
                ]
                [ Icon.view Icon.Notification
                ]
            , if config.notificationCount > 0 then
                Html.div
                    [ css [ paddingLeft (px 4) ]
                    ]
                    [ Html.text <| toString config.notificationCount ]
              else
                Html.text ""
            ]
        ]
