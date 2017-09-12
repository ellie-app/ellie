module Views.Editor.About.Styles exposing (..)

import Css exposing (..)
import Css.Namespace
import Shared.Colors as Colors
import Views.Editor.About.View exposing (CssClasses(..), namespace)


styles : Stylesheet
styles =
    (stylesheet << Css.Namespace.namespace namespace)
        [ Css.class Popout
            [ position absolute
            , backgroundColor (hex Colors.mediumGray)
            , width (px 400)
            , boxShadow5 (px -6) (px 6) (px 15) (px -4) (rgba 0 0 0 0.5)
            , property "z-index" "4"
            , borderRadius (px 3)
            , padding (px 16)
            , overflowY auto
            , border3 (px 2) solid (hex Colors.lightGray)
            , color (hex Colors.white)
            , right (px 16)
            , top (px 16)
            ]
        , Css.class Title
            [ fontSize (px 18)
            , fontWeight (int 700)
            ]
        , Css.class Paragraph
            [ fontSize (px 16)
            ]
        , Css.class Link
            [ color inherit
            , textDecoration none
            , fontWeight (int 700)
            ]
        ]
