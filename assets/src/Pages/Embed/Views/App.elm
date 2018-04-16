module Pages.Embed.Views.App exposing (styles, view)

import Colors
import Css exposing (..)
import Css.Foreign
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.Logo as Logo
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Pages.Embed.State.App as AppState exposing (Model(..), Msg)


view : Model -> Html Msg
view model =
    Html.styled Html.main_
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , displayFlex
        , flexDirection column
        , overflow hidden
        ]
        []
        [ viewHeader
        , viewContent
        ]


viewHeader : Html msg
viewHeader =
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
            , alignItems center
            ]
            []
            [ viewTab "Elm" True
            , viewTab "Html" False
            , viewTab "Result" False
            ]
        , Html.styled Html.a
            [ color Theme.primaryForeground
            , fontSize (px 16)
            , lineHeight (num 1)
            , Css.Foreign.children
                [ Css.Foreign.mediaQuery [ "(max-width: 320px)" ]
                    [ Css.Foreign.selector "[data-extraneous]"
                        [ display none |> important ]
                    ]
                ]
            ]
            []
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


viewTab : String -> Bool -> Html msg
viewTab label active =
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
        , if active then
            borderBottom3 (px 2) solid Theme.tabActiveBorder
          else
            batch []
        ]
        []
        [ Html.text label
        ]


viewContent : Html msg
viewContent =
    Html.styled Html.div
        [ height (pct 100)
        , flexShrink (int 1)
        ]
        []
        [ viewElmCode "module Main"
        ]


viewElmCode : String -> Html msg
viewElmCode code =
    CodeEditor.view
        [ CodeEditor.mode "elm"
        , CodeEditor.value code
        , CodeEditor.readOnly
        ]



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
