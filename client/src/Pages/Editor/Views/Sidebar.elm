module Pages.Editor.Views.Sidebar exposing (..)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


view : Html msg
view =
    Html.node "ellie-sidebar"
        [ css
            [ displayFlex
            , width (px 60)
            , backgroundColor Colors.darkMediumGray
            , borderRight3 (px 2) solid Colors.darkGray
            , flexDirection column
            , justifyContent spaceBetween
            , flexShrink (int 0)
            ]
        ]
        [ Html.div []
            [ viewLogo
            , viewIconButton Icon.Package
            , viewIconButton Icon.Settings
            , viewIconButton Icon.Help
            ]
        , Html.div []
            [ viewIconButton Icon.Slack
            , viewIconButton Icon.Trello
            , viewIconButton Icon.GitHub
            ]
        ]


viewLogo : Html msg
viewLogo =
    Html.div
        [ css
            [ width (px 58)
            , height (px 58)
            , padding4 (px 16) (px 16) (px 12) (px 16)
            , color Colors.lightGray
            ]
        ]
        [ Icon.view Icon.SmallLogo
        ]


viewIconButton : Icon.Icon -> Html msg
viewIconButton icon =
    Html.button
        [ css
            [ backgroundColor transparent
            , border zero
            , outline zero
            , padding2 (px 12) (px 16)
            , width (px 58)
            , height (px 50)
            , color Colors.lightGray
            , display block
            , cursor pointer
            ]
        ]
        [ Icon.view icon
        ]
