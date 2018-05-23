module Pages.Editor.Views.Setup
    exposing
        ( Stage(..)
        , view
        )

import Css exposing (..)
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Logo as Logo
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)


type Stage msg
    = Authenticating
    | Attaching
    | AcceptingTerms { termsVersion : Int, onAccept : msg }
    | Loading
    | Opening


view : Stage msg -> Html msg
view loadingStage =
    Html.div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , flexDirection column
            , height (pct 100)
            , position relative
            ]
        ]
    <|
        case loadingStage of
            AcceptingTerms state ->
                terms state

            _ ->
                [ Html.styled Html.div
                    [ width (pct 80)
                    , maxWidth (px 500)
                    , position relative
                    ]
                    []
                    [ Logo.animated ]
                ]


terms : { termsVersion : Int, onAccept : msg } -> List (Html msg)
terms state =
    [ Html.styled Html.div
        [ padding (px 16)
        , color Theme.primaryForeground
        , fontSize (px 24)
        , textAlign center
        ]
        []
        [ Html.text "Please accept Ellie's "
        , Html.styled Html.a
            [ color Theme.accent ]
            [ Attributes.href <| "/a/terms/" ++ toString state.termsVersion
            , Attributes.target "_blank"
            ]
            [ Html.text "Terms of Service" ]
        , Html.text " to continue."
        ]
    , Html.styled Html.div
        [ width (px 700)
        , maxWidth (pct 100)
        , position relative
        , padding (px 16)
        ]
        []
        [ Html.styled Html.iframe
            [ border zero
            , backgroundColor (hex "#fff")
            , width (pct 100)
            , position relative
            , height (px 400)
            ]
            [ Attributes.src <| "/a/terms/" ++ toString state.termsVersion
            ]
            []
        ]
    , Html.styled Html.div
        [ padding (px 16)
        , color Theme.primaryForeground
        , fontSize (px 18)
        , textAlign center
        , width (px 700)
        , maxWidth (pct 100)
        ]
        []
        [ Html.text "Ellie does not collect any personal information. All code posted to Ellie is public, licensed MIT. "
        , Html.text "Do not post proprietary or personally identifiable information on Ellie. "
        , Html.text "Report abuse or request removal of personal information at "
        , Html.styled Html.a
            [ color Theme.accent ]
            [ Attributes.href <| "mailto:ellie@lukewestby.com" ]
            [ Html.text "ellie@lukewestby.com" ]
        , Html.text ". See our "
        , Html.styled Html.a
            [ color Theme.accent ]
            [ Attributes.href <| "/a/terms/" ++ toString state.termsVersion ++ "#privacy"
            , Attributes.target "_blank"
            ]
            [ Html.text "Privacy Policy" ]
        , Html.text " for more details."
        ]
    , Html.div []
        [ Button.view
            { icon = Just Icon.Success
            , label = "Accept Terms"
            , action = Button.click state.onAccept
            }
        ]
    ]
