module Pages.Editor.Views.StatusBar exposing (Config, view)

import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)


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
            , justifyContent spaceBetween
            , alignItems center
            , displayFlex
            , backgroundColor Theme.primaryBackground
            , padding2 zero (px 12)
            , position relative
            ]
        ]
        [ Html.styled Html.div
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
            []
            [ Html.styled Html.a
                [ marginRight (px 16)
                , color Theme.primaryForeground
                , textDecoration none
                , fontSize (px 16)
                ]
                [ Attributes.href "/a/terms/4#privacy"
                , Attributes.target "_self"
                ]
                [ Html.text "Privacy" ]
            , Html.styled Html.a
                [ color Theme.primaryForeground
                , textDecoration none
                , fontSize (px 16)
                ]
                [ Attributes.href "/a/terms/4#terms"
                , Attributes.target "_self"
                ]
                [ Html.text "Terms" ]
            ]
        , Html.styled Html.div
            [ displayFlex
            , justifyContent center
            , alignItems center
            ]
            []
            [ Html.styled Html.div
                [ color Theme.primaryForeground
                , fontSize (px 16)
                , paddingRight (px 16)
                ]
                []
                [ Html.text config.compileStatus ]
            , Html.styled Html.div
                [ width (px 20)
                , height (px 20)
                , color <|
                    if config.connected then
                        Theme.connectionStatusConnected

                    else
                        Theme.connectionStatusDisconnected
                ]
                [ Attributes.title "Server connection status" ]
                [ Icon.view Icon.Socket ]
            , Html.styled Html.div
                [ displayFlex
                , alignItems center
                , paddingLeft (px 16)
                , color Theme.primaryForeground
                ]
                [ Attributes.title <|
                    String.fromInt config.notificationCount
                        ++ " notification"
                        ++ (if config.notificationCount == 1 then
                                ""

                            else
                                "s"
                           )
                ]
                [ Html.styled Html.div
                    [ width (px 20)
                    , height (px 20)
                    , color Theme.primaryForeground
                    ]
                    []
                    [ Icon.view Icon.Notification ]
                , if config.notificationCount > 0 then
                    Html.styled Html.div
                        [ paddingLeft (px 4) ]
                        []
                        [ Html.text <| String.fromInt config.notificationCount ]

                  else
                    Html.text ""
                ]
            ]
        ]
