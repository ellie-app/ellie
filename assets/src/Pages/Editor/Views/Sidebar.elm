module Pages.Editor.Views.Sidebar exposing (view, viewIconButton, viewIconLink, viewLogo)

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
            , viewIconButton Icon.Package "Package Manager" Actions.packages current
            , viewIconButton Icon.Settings "Settings" Actions.Settings current
            ]
        , Html.div []
            [ viewIconLink Icon.Slack "https://elmlang.slack.com/channels/ellie"
            , viewIconLink Icon.GithubProjects "https://github.com/ellie-app/ellie/projects"
            , viewIconLink Icon.GitHub "https://github.com/ellie-app/ellie"
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


viewIconButton : Icon.Icon -> String -> Actions.Model -> Actions.Model -> Html WorkingState.Msg
viewIconButton icon tooltip default current =
    let
        ( nextModel, isActive ) =
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
                if isActive then
                    Theme.primaryForeground

                else
                    Theme.secondaryForeground
            , hover
                [ color Theme.primaryForeground ]
            , active
                [ transform <| scale 1.1 ]
            ]
        , onClick <| WorkingState.ActionPaneSelected nextModel
        , Attributes.title tooltip
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
