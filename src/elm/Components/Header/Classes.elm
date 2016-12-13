module Components.Header.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Header
    | Logo
    | LogoText
    | Status
    | StatusText
    | StatusEllipsis
    | Button
    | ButtonIcon


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "views"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
