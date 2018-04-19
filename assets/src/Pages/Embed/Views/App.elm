module Pages.Embed.Views.App exposing (styles, view)

import Colors
import Css exposing (..)
import Css.Foreign
import Data.Url as Url
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.Logo as Logo
import Ellie.Ui.Output as Output
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Pages.Embed.State.App as AppState exposing (Model(..), Msg(..), OutputState, WorkingState)
import Pages.Embed.Types.Panel as Panel exposing (Panel)
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)


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
    Html.main_ []
        [ Html.text "loading"
        ]


viewFailure : Html msg
viewFailure =
    Html.main_ []
        [ Html.text "failed"
        ]


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


viewHeader : RevisionId -> Panel -> Html Msg
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
            , viewTab Panel.Debugger current
            ]
        , Html.styled Html.a
            [ color Theme.primaryForeground
            , fontSize (px 16)
            , lineHeight (num 1)
            , textDecoration none
            , hover [ color Theme.accent ]
            , Css.Foreign.children
                [ Css.Foreign.mediaQuery [ "(max-width: 320px)" ]
                    [ Css.Foreign.selector "[data-extraneous]"
                        [ display none |> important ]
                    ]
                ]
            ]
            [ Attributes.href <| Url.toString <| RevisionId.editorLink revisionId ]
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
    case ( panelForOutput state.panel, state.output ) of
        ( True, AppState.NotRun ) ->
            Html.styled Html.div
                [ position relative
                , width (pct 100)
                , height (pct 100)
                ]
                [ Events.onClick EmbedRunStarted ]
                [ Html.text "Click to run"
                ]

        ( True, AppState.AcquiringConnection ) ->
            Html.div [] [ Html.text "Connecting" ]

        ( True, AppState.Compiling ) ->
            Html.div [] [ Html.text "Compiling" ]

        ( True, AppState.Crashed message ) ->
            Html.div [] [ Html.text "Crashed" ]

        ( True, AppState.Finished (Just error) ) ->
            Html.div [] [ Html.text "Errors" ]

        ( _, AppState.Finished Nothing ) ->
            Html.styled Html.div
                [ width (pct 100)
                , height (pct 100)
                , position relative
                ]
                []
                [ Output.view
                    [ Output.html state.revision.data.htmlCode
                    , Output.debug (Panel.eq Panel.Debugger state.panel)
                    , Output.elmSource <| Url.toString <| RevisionId.outputLink state.revision.id
                    ]
                ]

        _ ->
            Html.text ""


panelForOutput : Panel -> Bool
panelForOutput panel =
    case panel of
        Panel.Output ->
            True

        Panel.Debugger ->
            True

        _ ->
            False



-- STYLES


styles : List Css.Foreign.Snippet
styles =
    [ Css.Foreign.html
        [ height (pct 100)
        , backgroundColor Theme.secondaryBackground
        ]
    , Css.Foreign.body
        [ height (pct 100)
        , margin zero
        , fontFamilies [ "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial", "sans-serif" ]
        , property "-webkit-font-smoothing" "antialiased"
        ]
    , Css.Foreign.everything
        [ boxSizing borderBox ]
    , Css.Foreign.button
        [ focus [ outline zero ] ]
    , Css.Foreign.input
        [ focus [ outline zero ] ]
    , Theme.darkStyles
    ]
