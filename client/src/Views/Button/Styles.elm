module Views.Button.Styles exposing (keyFrames, styles)

import Css exposing (..)
import Css.Namespace exposing (..)
import Extra.Css.Animation as Animation exposing (KeyFrames, animation, ease, from, infinite, to)
import Shared.Colors as Colors
import Time
import Views.Button.View as View exposing (CssClass(..))


flash : KeyFrames
flash =
    Animation.keyFrames
        [ ( from, [ opacity (num 0.2) ] )
        , ( to, [ opacity (num 0.6) ] )
        ]


keyFrames : List KeyFrames
keyFrames =
    [ flash
    ]


styles : Stylesheet
styles =
    (stylesheet << namespace View.namespace)
        [ Css.class Button
            [ display inlineBlock
            , backgroundColor (hex Colors.darkGray)
            , color (hex Colors.lightGray)
            , borderRadius (px 3)
            , border zero
            , padding (px 12)
            , fontSize (px 16)
            , textTransform uppercase
            , fontFamily inherit
            , fontWeight (int 500)
            , cursor pointer
            , Css.withClass Loading
                [ animation (1 * Time.second) ease 0 infinite "alternate" "both" "running" flash
                , opacity (num 0.6)
                ]
            ]
        , Css.class LoadingFiller
            [ display inlineBlock
            , height (em 1)
            ]
        ]
