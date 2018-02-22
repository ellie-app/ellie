module Pages.Editor.Views.Workbench exposing (..)

import Css exposing (..)
import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Pages.Editor.State.Working exposing (Compilation(..), WorkbenchPane(..))


type alias Config msg =
    { onCompile : msg
    , onSelectPane : WorkbenchPane -> msg
    , compilation : Compilation
    , pane : Maybe WorkbenchPane
    , token : Jwt
    }


view : Config msg -> Html msg
view config =
    Html.div
        [ css
            [ backgroundColor Theme.primaryBackground
            , width (pct 100)
            , height (pct 100)
            , position relative
            , zIndex (int 0)
            ]
        ]
        [ viewWatermark
        , Html.div
            [ css
                [ zIndex (int 1)
                , position relative
                , width (pct 100)
                , height (pct 100)
                , displayFlex
                , flexDirection column
                ]
            ]
            [ Html.div
                []
                [ Html.button [ Events.onClick config.onCompile ] [ Html.text "Compile" ]
                , Html.button [ Events.onClick (config.onSelectPane Output) ] [ Html.text "Output" ]
                , Html.button [ Events.onClick (config.onSelectPane Debug) ] [ Html.text "Debug" ]
                ]
            , case config.pane of
                Nothing ->
                    viewInitial config

                Just Output ->
                    viewOutput config

                Just Debug ->
                    viewOutput config
            ]
        ]


viewInitial : Config msg -> Html msg
viewInitial config =
    Html.div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent center
            , width (pct 100)
            , height (pct 100)
            ]
        ]
        [ Html.div
            [ css
                [ color Theme.primaryForeground
                , fontSize (px 24)
                ]
            ]
            [ Html.text "Press COMPILE to run your program" ]
        , Html.div
            [ css [ paddingTop (px 24) ]
            ]
            [ Button.view
                { icon = Just Icon.Play
                , label = "Compile"
                , disabled = config.compilation == Compiling
                , action = Button.click config.onCompile
                }
            ]
        ]


viewOutput : Config msg -> Html msg
viewOutput config =
    Html.iframe
        [ Attributes.src <| "/private-api/workspace/result/html?token=" ++ Jwt.toString config.token
        , Attributes.id "workbenchIframe"
        , css
            [ width (pct 100)
            , height (pct 100)
            , backgroundColor (hex "#fff")
            , border zero
            ]
        ]
        []


viewWatermark : Html msg
viewWatermark =
    Html.div
        [ css
            [ property "pointer-events" "none"
            , color Theme.workbenchWatermark
            , width (pct 100)
            , height (pct 100)
            , displayFlex
            , alignItems center
            , justifyContent center
            , overflow hidden
            , position absolute
            , zIndex (int 0)
            , top zero
            , left zero
            ]
        ]
        [ Html.div
            [ css
                [ minWidth (px 600)
                , flexShrink zero
                ]
            ]
            [ Icon.view Icon.SmallLogo ]
        ]
