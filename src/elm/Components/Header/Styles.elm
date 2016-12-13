module Components.Header.Styles exposing (..)

import Css exposing (..)
import Css.Namespace exposing (..)
import Css.Elements exposing (..)
import Components.Header.Classes exposing (Classes(..))
import Shared.Constants as Constants


styles : Stylesheet
styles =
    (stylesheet << namespace "views")
        [ (.) Header
            [ width (pct 100)
            , height (px 60)
            , backgroundColor (hex "f7f7f7")
            , borderBottom3 (px 1) solid (hex "55B5DB")
            , displayFlex
            , alignItems center
            ]
        , (.) Logo
            [ padding2 zero (px 16)
            ]
        , (.) LogoText
            [ property "font-family" "Leckerli One"
            , margin zero
            ]
        , (.) Status
            [ padding2 zero (px 16)
            ]
        , (.) StatusText
            [ textTransform uppercase
            , fontSize (em 1.1)
            ]
        , (.) Button
            [ property "-webkit-appearance" "none"
            , backgroundColor transparent
            , border zero
            , padding2 zero (px 10)
            , fontFamily inherit
            , fontSize (px 13)
            , textTransform uppercase
            , fontWeight inherit
            , cursor pointer
            , outline zero
            , height (pct 100)
            , borderBottom3 (px 2) solid transparent
            , displayFlex
            , alignItems center
            , property "justify-content" "center"
            , property "fill" "currentColor"
            , hover
                [ borderBottomColor (hex "55B5DB") ]
            , disabled
                [ color (hex "A7A7A7")
                , cursor wait
                , hover [ borderBottomColor (hex "A7A7A7") ]
                ]
            ]
        , (.) ButtonIcon
            [ height (px 13)
            , width (px 13)
            , display inlineBlock
            , marginRight (px 6)
            , position relative
            ]
        ]
