module Pages.Editor.Views.Loading
    exposing
        ( Stage(..)
        , view
        )

import Colors
import Css exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


type Stage
    = Authenticating
    | CreatingWorkspace


view : Stage -> Html msg
view loadingStage =
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , flexDirection column
            , height (pct 100)
            , position relative
            ]
        ]
        [ logo
        , div
            [ css
                [ fontSize (px 32)
                , padding2 (px 12) zero
                , color Colors.lightGray
                ]
            ]
            [ case loadingStage of
                Authenticating ->
                    text "Authenticating..."

                CreatingWorkspace ->
                    text "Attaching to workspace..."
            ]
        ]


logo : Html msg
logo =
    Svg.svg []
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
