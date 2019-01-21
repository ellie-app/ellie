module Pages.Embed.Views.App exposing (styles, title, view)

import Css exposing (..)
import Css.Global
import Ellie.Ui.Button as Button
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.Errors as Errors
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Logo as Logo
import Ellie.Ui.Output as Output
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Pages.Embed.State.App as AppState exposing (Model(..), Msg(..), OutputState, WorkingState)
import Pages.Embed.Types.Panel as Panel exposing (Panel)
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Url as Url


title : Model -> String
title model =
    case model of
        Working workingState ->
            case workingState.revision.data.title of
                "" ->
                    "Ellie - Untitled"

                name ->
                    "Ellie - " ++ name

        _ ->
            "Ellie"


view : Model -> Html Msg
view model =
    case model of
        Loading _ _ ->
            viewLoading

        Failure ->
            viewFailure

        Working state ->
            viewWorking state


viewLoading : Html msg
viewLoading =
    Html.styled Html.main_
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , displayFlex
        , justifyContent center
        , alignItems center
        , overflow hidden
        ]
        []
        [ Html.styled Html.div
            [ maxWidth (px 300)
            , height (px 200)
            , width (pct 80)
            ]
            []
            [ Logo.animated
            ]
        ]


viewFailure : Html msg
viewFailure =
    viewCrashed "Something went wrong while loading Ellie. If you are having trouble with your network you can try to reload. If your network is fine then I'm not sure what exactly went wrong, but any server issues have been automatically reported!"


viewWorking : WorkingState -> Html Msg
viewWorking state =
    Html.styled Html.main_
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , displayFlex
        , flexDirection column
        , overflow hidden
        ]
        []
        [ viewHeader state.revision.id state.panel
        , viewContent state
        ]


viewHeader : Revision.Id -> Panel -> Html Msg
viewHeader revisionId current =
    Html.styled Html.header
        [ height (px 40)
        , backgroundColor Theme.primaryBackground
        , borderBottom3 (px 2) solid Theme.staticBorder
        , displayFlex
        , alignItems center
        , justifyContent spaceBetween
        , padding2 (px 0) (px 16)
        , flexShrink zero
        ]
        []
        [ Html.styled Html.nav
            [ displayFlex
            , alignItems flexStart
            ]
            []
            [ viewTab Panel.Elm current
            , viewTab Panel.Html current
            , viewTab Panel.Output current
            ]
        , Html.styled Html.a
            [ color Theme.primaryForeground
            , fontSize (px 16)
            , lineHeight (num 1)
            , textDecoration none
            , hover [ color Theme.accent ]
            , Css.Global.children
                [ Css.Global.mediaQuery [ "(max-width: 320px)" ]
                    [ Css.Global.selector "[data-extraneous]"
                        [ display none |> important ]
                    ]
                ]
            ]
            [ Attributes.href <| Url.toString <| Revision.editorLink revisionId
            , Attributes.target "_blank"
            ]
            [ Html.span [] [ Html.text "Edit" ]
            , Html.span
                [ Attributes.attribute "data-extraneous" "" ]
                [ Html.text " on " ]
            , Html.styled Html.span
                [ height (px 16)
                , width (px 45)
                , display inlineBlock
                , verticalAlign bottom
                ]
                [ Attributes.attribute "data-extraneous" "" ]
                [ Logo.flat ]
            ]
        ]


viewTab : Panel -> Panel -> Html Msg
viewTab mine current =
    Html.styled Html.button
        [ border zero
        , outline zero
        , property "background" "none"
        , fontFamily inherit
        , color Theme.tabForeground
        , fontWeight bold
        , fontSize (px 18)
        , padding (px 3)
        , marginRight (px 12)
        , lineHeight (num 1)
        , cursor pointer
        , textTransform capitalize
        , if Panel.eq mine current then
            borderBottom3 (px 2) solid Theme.tabActiveBorder

          else
            batch []
        ]
        [ Events.onClick (PanelSelected mine) ]
        [ Html.text (Panel.toString mine)
        ]


viewContent : WorkingState -> Html Msg
viewContent state =
    Html.styled Html.div
        [ height (pct 100)
        , flexShrink (int 1)
        , position relative
        , displayFlex
        ]
        []
        [ viewOutput state
        , case state.panel of
            Panel.Elm ->
                viewOverlayed <|
                    CodeEditor.view
                        [ CodeEditor.mode "elm"
                        , CodeEditor.value state.revision.data.elmCode
                        , CodeEditor.readOnly
                        ]

            Panel.Html ->
                viewOverlayed <|
                    CodeEditor.view
                        [ CodeEditor.mode "htmlmixed"
                        , CodeEditor.value state.revision.data.htmlCode
                        , CodeEditor.readOnly
                        ]

            _ ->
                Html.text ""
        ]


viewOverlayed : Html msg -> Html msg
viewOverlayed child =
    Html.styled Html.div
        [ position absolute
        , top zero
        , left zero
        , width (pct 100)
        , height (pct 100)
        ]
        []
        [ child ]


viewOutput : WorkingState -> Html Msg
viewOutput state =
    case ( state.panel, state.output ) of
        ( Panel.Output, AppState.NotRun ) ->
            viewClickToRun

        ( Panel.Output, AppState.AcquiringConnection ) ->
            viewAnimatedContainer "Connecting"

        ( Panel.Output, AppState.Compiling ) ->
            viewAnimatedContainer "Compiling"

        ( Panel.Output, AppState.Crashed message ) ->
            viewCrashed message

        ( Panel.Output, AppState.Finished (Just error) ) ->
            Errors.view
                { error = error
                , onPositionClick = GoToPosition
                }

        ( _, AppState.Finished Nothing ) ->
            Html.styled Html.div
                [ width (pct 100)
                , height (pct 100)
                , position relative
                , displayFlex
                , flexDirection column
                , overflow hidden
                ]
                []
                [ Html.styled Html.div
                    [ height (pct 100)
                    , position relative
                    , flexShrink (int 1)
                    , overflow hidden
                    ]
                    []
                    [ Output.view
                        [ Output.html state.revision.data.htmlCode
                        , Output.elmSource <| Url.toString <| Revision.outputLink state.revision.id
                        , Output.onCanDebug CanDebugChanged
                        , Output.debug (state.debug == AppState.Debugging)
                        ]
                    ]
                , viewControls state
                ]

        _ ->
            Html.text ""


viewControls : WorkingState -> Html Msg
viewControls state =
    Html.styled Html.div
        [ height (px 40)
        , backgroundColor Theme.secondaryBackground
        , width (pct 100)
        , flexShrink (int 0)
        , displayFlex
        , alignItems center
        , padding2 zero (px 12)
        ]
        []
        [ Html.styled Html.div
            [ marginRight (px 8) ]
            []
            [ Button.view
                { icon = Just Icon.Reload
                , label = "Reload"
                , action = Button.click ReloadOutput
                }
            ]
        , case state.debug of
            AppState.DebuggerUnavailable ->
                Html.text ""

            AppState.Debugging ->
                Button.view
                    { icon = Just Icon.Eye
                    , label = "View Output"
                    , action = Button.click (ToggleDebugger False)
                    }

            AppState.NotDebugging ->
                Button.view
                    { icon = Just Icon.Debugger
                    , label = "View Debugger"
                    , action = Button.click (ToggleDebugger True)
                    }
        ]


viewClickToRun : Html Msg
viewClickToRun =
    Html.styled Html.div
        [ position relative
        , width (pct 100)
        , height (pct 100)
        , fontSize (px 36)
        , displayFlex
        , alignItems center
        , fontWeight bold
        , color Theme.primaryForeground
        , cursor pointer
        , flexDirection column
        , justifyContent center
        ]
        [ Events.onClick EmbedRunStarted ]
        [ Html.styled Html.div
            [ width (pct 80)
            , maxWidth (px 300)
            , marginBottom (px 24)
            ]
            []
            [ Logo.flat ]
        , Html.text "Click to Run"
        ]


viewAnimatedContainer : String -> Html msg
viewAnimatedContainer text =
    Html.styled Html.div
        [ position relative
        , width (pct 100)
        , height (pct 100)
        , fontSize (px 36)
        , displayFlex
        , alignItems center
        , fontWeight bold
        , color Theme.primaryForeground
        , cursor pointer
        , flexDirection column
        , justifyContent center
        ]
        []
        [ Html.styled Html.div
            [ width (pct 80)
            , maxWidth (px 300)
            , marginBottom (px 24)
            ]
            []
            [ Logo.animated ]
        , Html.text text
        ]


viewCrashed : String -> Html msg
viewCrashed message =
    Html.styled Html.div
        [ position relative
        , width (pct 100)
        , height (pct 100)
        , displayFlex
        , alignItems center
        , justifyContent center
        , color Theme.primaryForeground
        , cursor pointer
        , flexDirection column
        , padding (px 32)
        ]
        []
        [ Html.styled Html.div
            [ fontSize (px 36)
            , fontWeight bold
            , marginBottom (px 24)
            , textAlign center
            ]
            []
            [ Html.text "Something went wrong!" ]
        , Html.styled Html.div
            [ fontSize (px 24)
            , textAlign center
            ]
            []
            [ Html.text message ]
        ]



-- STYLES


styles : List Css.Global.Snippet
styles =
    [ Css.Global.html
        [ height (pct 100)
        , backgroundColor Theme.secondaryBackground
        ]
    , Css.Global.body
        [ height (pct 100)
        , margin zero
        , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif" ]
        , property "-webkit-font-smoothing" "antialiased"
        ]
    , Css.Global.everything
        [ boxSizing borderBox ]
    , Css.Global.button
        [ focus [ outline zero ] ]
    , Css.Global.input
        [ focus [ outline zero ] ]
    , Theme.darkStyles
    ]
