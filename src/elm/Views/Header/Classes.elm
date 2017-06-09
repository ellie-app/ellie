module Views.Header.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Header
    | HeaderGroup
    | Logo
    | Button
    | ButtonIcon
    | ButtonText
    | ButtonInner


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_header_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
