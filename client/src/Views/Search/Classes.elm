module Views.Search.Classes
    exposing
        ( Classes(..)
        , class
        , classList
        )

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Container
    | Backdrop
    | SearchBar
    | SearchBarInput
    | SearchBarIcon
    | Results
    | ResultsItem
    | ResultsItemInfo
    | ResultsItemName
    | ResultsItemVersion
    | ResultsItemButtonGroup
    | ResultsItemButton
    | ResultsItemButtonInner
    | ResultsItemButtonIcon
    | ResultsItemButtonText


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_search_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
