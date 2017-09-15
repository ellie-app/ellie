module Views.Editor.Terms.Styles exposing (styles)

import Css exposing (..)
import Css.Namespace as Namespace
import Shared.Colors as Colors
import Views.Editor.Terms.View exposing (..)


styles : Stylesheet
styles =
    (stylesheet << Namespace.namespace namespace)
        [ class Popout
            [ padding (px 12)
            , backgroundColor (hex Colors.white)
            , position absolute
            , width (px 300)
            , zIndex (int 2)
            , left (px 228)
            , top (px 16)
            , borderRadius (px 3)
            , border3 (px 2) solid (hex Colors.lightGray)
            , backgroundColor (hex Colors.mediumGray)
            , color (hex Colors.white)
            , textAlign center
            ]
        , class Content
            [ paddingBottom (px 12)
            ]
        , class Link
            [ color (hex Colors.white)
            , fontWeight bold
            , textDecoration none
            ]
        ]
