module Components.PackageSearch.Classes
    exposing
        ( Classes(..)
        , class
        , classList
        )

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = ActionIcon


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_packageSearch_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
