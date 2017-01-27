module Components.Editor.EmbedLink.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Container


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_editor_embedLink_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
