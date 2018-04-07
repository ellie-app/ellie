module Pages.Editor.Views.Setup
    exposing
        ( Stage(..)
        , view
        )

import Css exposing (..)
import Data.Url as Url
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


type Stage msg
    = Authenticating
    | Attaching
    | AcceptingTerms { termsVersion : TermsVersion, onAccept : msg, loading : Bool }
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
                [ logo ]


terms : { termsVersion : TermsVersion, onAccept : msg, loading : Bool } -> List (Html msg)
terms state =
    [ Html.styled Html.div
        [ padding (px 16)
        , color Theme.primaryForeground
        , fontSize (px 18)
        , textAlign center
        ]
        []
        [ Html.text "Please accept Ellie's "
        , Html.styled Html.a
            [ color Theme.accent ]
            [ Attributes.href <| Url.toString <| TermsVersion.link state.termsVersion
            , Attributes.target "_blank"
            ]
            [ Html.text "Terms of Service" ]
        , Html.text " to continue."
        ]
    , Html.styled Html.div
        [ width (px 532)
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
            , disabled = False
            , action = Button.click state.onAccept
            }
        ]
    ]


logo : Html msg
logo =
    Svg.svg
        []
        [ Svg.defs []
            [ Svg.mask [ SvgAttributes.id "clip" ]
                [ Svg.use
                    [ SvgAttributes.width "100%"
                    , SvgAttributes.height "100%"
                    , SvgAttributes.fill "#fff"
                    , SvgAttributes.xlinkHref "#ellie-logo"
                    , SvgAttributes.x "0"
                    , SvgAttributes.y "0"
                    ]
                    []
                ]
            , Svg.linearGradient
                [ SvgAttributes.id "shimmer" ]
                [ Svg.stop [ SvgAttributes.stopColor "#fff", SvgAttributes.stopOpacity "0", SvgAttributes.offset "0%" ] []
                , Svg.stop [ SvgAttributes.stopColor "#fff", SvgAttributes.stopOpacity "0.4", SvgAttributes.offset "50%" ] []
                , Svg.stop [ SvgAttributes.stopColor "#fff", SvgAttributes.stopOpacity "0", SvgAttributes.offset "65%" ] []
                ]
            , Svg.linearGradient
                [ SvgAttributes.id "background", SvgAttributes.gradientTransform "rotate(30) scale(2)" ]
                [ Svg.stop [ SvgAttributes.stopColor "#FC6ECC", SvgAttributes.offset "15%" ] []
                , Svg.stop [ SvgAttributes.stopColor "#5500FF", SvgAttributes.offset "60%" ] []
                ]
            ]
        , Svg.g [ SvgAttributes.style "mask: url(#clip)" ]
            [ Svg.rect [ SvgAttributes.width "100%", SvgAttributes.height "100%", SvgAttributes.fill "url(#background)" ] []
            , Svg.rect
                [ SvgAttributes.css
                    [ width (pct 100)
                    , height (pct 100)
                    , property "fill" "url(#shimmer)"
                    , property "animation" "shimmer 2s infinite cubic-bezier(0.25, 0.46, 0.45, 0.94)"
                    ]
                ]
                []
            ]
        ]
