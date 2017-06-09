module Views.Editors.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Loading
    | LoadingGutter
    | LoadingLineNumber
    | LoadingLine
    | LoadingLines
    | LoadingShimmer


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_editors_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
