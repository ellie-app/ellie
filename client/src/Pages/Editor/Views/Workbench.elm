module Pages.Editor.Views.Workbench exposing (..)

import BoundedDeque exposing (BoundedDeque)
import Css exposing (..)
import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Types.Revision exposing (Revision)
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.TextInput as TextInput
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Html.Styled.Keyed as Keyed
import Pages.Editor.State.Working exposing (ErrorsPane(..), SuccessPane(..), Workbench(..))
import Pages.Editor.Types.Log as Log exposing (Log)
import Pages.Editor.Views.Output as Output


type alias Config msg =
    { onCompile : msg
    , onExpand : msg
    , onIframeReload : msg
    , onClearLogs : msg
    , onLogSearchChanged : String -> msg
    , onLogReceived : Log -> msg
    , onSelectErrorsPane : ErrorsPane -> msg
    , onSelectSuccessPane : SuccessPane -> msg
    , compiling : Bool
    , workbench : Workbench
    , maximized : Bool
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
                ]
            ]
            [ viewContent config ]
        ]


viewContent : Config msg -> Html msg
viewContent config =
    case config.workbench of
        Ready ->
            viewInitial config

        Finished { pane, logs, logSearch } ->
            Html.div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , height (pct 100)
                    , width (pct 100)
                    , position relative
                    ]
                ]
                [ viewFinishedHeader pane config
                , Html.div
                    [ css
                        [ height (pct 100)
                        , width (pct 100)
                        , position relative
                        , displayFlex
                        ]
                    ]
                    [ viewOutput (pane == SuccessDebug) config
                    , case pane of
                        SuccessLogs ->
                            viewLogs logSearch logs config

                        _ ->
                            Html.text ""
                    ]
                ]

        FinishedWithErrors _ ->
            Html.text ""


viewFinishedHeader : SuccessPane -> Config msg -> Html msg
viewFinishedHeader pane config =
    Html.div
        [ css
            [ width (pct 100)
            , height (px 42)
            , displayFlex
            , backgroundColor Theme.primaryBackground
            , flexShrink zero
            , alignItems center
            , justifyContent flexEnd
            , padding2 zero (px 8)
            , overflow hidden
            , borderBottom3 (px 2) solid Theme.staticBorder
            ]
        ]
        [ Html.button
            [ Attributes.title <|
                if config.maximized then
                    "Minimize workbench"
                else
                    "Maximize workbench"
            , Events.onClick config.onExpand
            , css
                [ border zero
                , property "background" "none"
                , outline zero
                , height (pct 100)
                , padding2 zero (px 8)
                , width (px 28)
                , flexShrink (int 0)
                , color Theme.secondaryForeground
                , hover [ color Theme.primaryForeground ]
                , cursor pointer
                ]
            ]
            [ Html.div
                [ if config.maximized then
                    css [ transform <| rotate (deg -90) ]
                  else
                    css [ transform <| rotate (deg 90) ]
                ]
                [ Icon.view Icon.Chevron ]
            ]
        , viewHeaderActions <|
            List.filterMap identity
                [ if config.compiling then
                    Just
                        { icon = Just Icon.Loading
                        , label = "Compiling..."
                        , disabled = True
                        , action = Button.none
                        }
                  else
                    Just
                        { icon = Just Icon.Play
                        , label = "Compile"
                        , disabled = False
                        , action = Button.click config.onCompile
                        }
                , case pane of
                    SuccessOutput ->
                        Just
                            { icon = Just Icon.Reload
                            , label = "Reload"
                            , disabled = False
                            , action = Button.click config.onIframeReload
                            }

                    SuccessDebug ->
                        Nothing

                    SuccessLogs ->
                        Just
                            { icon = Just Icon.Trash
                            , label = "Clear"
                            , disabled = False
                            , action = Button.click config.onClearLogs
                            }

                    SuccessShare ->
                        Nothing
                ]
        , viewHeaderTabs
            [ ( config.onSelectSuccessPane SuccessOutput, "Output", pane == SuccessOutput )
            , ( config.onSelectSuccessPane SuccessDebug, "Debug", pane == SuccessDebug )
            , ( config.onSelectSuccessPane SuccessLogs, "Logs", pane == SuccessLogs )
            ]
        ]


viewHeaderActions : List (Button.Config msg) -> Html msg
viewHeaderActions actionDefs =
    Html.div
        [ css
            [ displayFlex
            , flexShrink (int 0)
            , overflowX auto
            , overflowY hidden
            , height (pct 100)
            , alignItems center
            , justifyContent flexStart
            ]
        ]
        (List.map viewHeaderAction actionDefs)


viewHeaderAction : Button.Config msg -> Html msg
viewHeaderAction config =
    Html.div
        [ css
            [ padding2 zero (px 8)
            , height (pct 100)
            , displayFlex
            , alignItems center
            ]
        ]
        [ Button.view config ]


viewHeaderTabs : List ( msg, String, Bool ) -> Html msg
viewHeaderTabs tabDefs =
    Html.div
        [ css
            [ displayFlex
            , flexShrink (int 1)
            , overflowX auto
            , width (pct 100)
            , height (pct 100)
            , alignItems center
            , justifyContent flexEnd
            ]
        ]
        (List.map (\( msg, name, active ) -> viewHeaderTab msg name active) tabDefs)


viewHeaderTab : msg -> String -> Bool -> Html msg
viewHeaderTab msg name active =
    Html.button
        [ css
            [ outline zero
            , border zero
            , property "background" "none"
            , padding2 zero (px 8)
            , displayFlex
            , alignItems center
            , height (pct 100)
            , cursor pointer
            ]
        , Events.onClick msg
        ]
        [ Html.div
            [ css
                [ fontSize (px 14)
                , color Theme.tabForeground
                , padding2 zero (px 2)
                , paddingBottom (px 3)
                , fontWeight bold
                , textTransform uppercase
                , if active then
                    borderBottom3 (px 1) solid Theme.tabActiveBorder
                  else
                    batch []
                ]
            ]
            [ Html.text name ]
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
            [ if config.compiling then
                Button.view
                    { icon = Just Icon.Loading
                    , label = "Compiling..."
                    , disabled = True
                    , action = Button.click config.onCompile
                    }
              else
                Button.view
                    { icon = Just Icon.Play
                    , label = "Compile"
                    , disabled = False
                    , action = Button.click config.onCompile
                    }
            ]
        ]


viewOutput : Bool -> Config msg -> Html msg
viewOutput debug config =
    Output.view
        [ Output.src <| "/private-api/workspace/result/html?token=" ++ Jwt.toString config.token
        , Output.onLog config.onLogReceived
        , Output.debug debug
        ]


viewLogs : String -> BoundedDeque Log -> Config msg -> Html msg
viewLogs logSearch logs config =
    let
        lowerSearch =
            String.toLower logSearch

        filteredLogs =
            if String.isEmpty lowerSearch then
                BoundedDeque.toList logs
            else
                logs
                    |> BoundedDeque.toList
                    |> List.filter (\l -> String.startsWith lowerSearch (String.toLower l.label))
    in
    Html.div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , backgroundColor Theme.secondaryBackground
            , position absolute
            , top zero
            , left zero
            , displayFlex
            , flexDirection column
            ]
        ]
        [ Html.div
            [ css
                [ padding (px 16)
                , flexShrink (int 0)
                ]
            ]
            [ TextInput.view
                { placeholder = "Filter by label"
                , value = logSearch
                , clearable = True
                , onChange = config.onLogSearchChanged
                , autofocus = False
                , icon = Just Icon.Search
                }
            ]
        , Html.div
            [ css
                [ padding2 zero (px 2)
                , overflowY auto
                ]
            ]
            (List.map viewLog filteredLogs)
        ]


viewLog : Log -> Html msg
viewLog log =
    Html.div
        [ css
            [ width (pct 100)
            , margin2 (px 2) zero
            , backgroundColor Theme.primaryBackground
            , padding (px 12)
            , firstChild [ marginTop zero ]
            , lastChild [ marginBottom zero ]
            ]
        ]
        [ Html.div
            [ css
                [ color Theme.secondaryForeground
                , fontSize (px 14)
                , fontWeight bold
                , paddingBottom (px 12)
                , lineHeight (num 1)
                ]
            ]
            [ Html.text log.label ]
        , Html.div
            [ css
                [ color Theme.primaryForeground
                , fontSize (px 18)
                , lineHeight (num 1)
                , fontFamily Theme.editorFontFamily
                , whiteSpace preWrap
                ]
            ]
            [ Html.text log.body ]
        ]


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
