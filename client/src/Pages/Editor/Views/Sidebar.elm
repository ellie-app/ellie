module Pages.Editor.Views.Sidebar exposing (..)

import Colors
import Css exposing (..)
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Pages.Editor.State.Actions as Actions
import Pages.Editor.State.Working as WorkingState


view : Actions.Model -> Html WorkingState.Msg
view current =
    Html.node "ellie-sidebar"
        [ css
            [ displayFlex
            , width (px 60)
            , backgroundColor Theme.primaryBackground
            , borderRight3 (px 2) solid Theme.staticBorder
            , flexDirection column
            , justifyContent spaceBetween
            , flexShrink (int 0)
            ]
        ]
        [ Html.div []
            [ viewLogo
            , viewIconButton Icon.Package Actions.packages current
            , viewIconButton Icon.Settings Actions.Settings current
            , viewIconButton Icon.Help Actions.Help current
            ]
        , Html.div []
            [ viewIconLink Icon.Slack "https://elmlang.slack.com/#ellie"
            , viewIconLink Icon.GithubProjects "https://github.com/lukewestby/ellie/projects"
            , viewIconLink Icon.GitHub "https://github.com/lukewestby/ellie"
            ]
        ]


viewLogo : Html msg
viewLogo =
    Html.div
        [ css
            [ width (px 58)
            , height (px 58)
            , padding4 (px 16) (px 16) (px 12) (px 16)
            , color Theme.primaryForeground
            ]
        ]
        [ Icon.view Icon.SmallLogo
        ]


viewIconButton : Icon.Icon -> Actions.Model -> Actions.Model -> Html WorkingState.Msg
viewIconButton icon default current =
    let
        ( nextModel, active ) =
            case ( default, current ) of
                ( Actions.Settings, Actions.Settings ) ->
                    ( Actions.Hidden, True )

                ( Actions.Packages _, Actions.Packages _ ) ->
                    ( Actions.Hidden, True )

                ( Actions.Help, Actions.Help ) ->
                    ( Actions.Hidden, True )

                _ ->
                    ( default, False )
    in
    Html.button
        [ css
            [ backgroundColor transparent
            , border zero
            , outline zero
            , padding2 (px 12) (px 16)
            , width (px 58)
            , height (px 50)
            , display block
            , cursor pointer
            , color <|
                if active then
                    Theme.primaryForeground
                else
                    Theme.secondaryForeground
            , hover
                [ color Theme.primaryForeground
                ]
            ]
        , onClick <| WorkingState.ActionPaneSelected nextModel
        ]
        [ Icon.view icon
        ]


viewIconLink : Icon.Icon -> String -> Html msg
viewIconLink icon url =
    Html.a
        [ Attributes.href url
        , Attributes.target "_blank"
        , css
            [ padding2 (px 12) (px 16)
            , width (px 58)
            , height (px 50)
            , color Theme.primaryForeground
            , display block
            , cursor pointer
            ]
        ]
        [ Icon.view icon
        ]
