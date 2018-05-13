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
    | AcceptingTerms { termsVersion : Int, onAccept : msg, loading : Bool }
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


terms : { termsVersion : Int, onAccept : msg, loading : Bool } -> List (Html msg)
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
            [ Attributes.src "/a/terms/1"
            ]
            []
        ]
    , Html.div []
        [ Button.view
            { icon =
                if state.loading then
                    Just Icon.Loading
                else
                    Just Icon.Success
            , label = "Accept Terms"
            , action = Button.click state.onAccept
            }
        ]
    ]
