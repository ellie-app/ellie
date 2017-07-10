module Views.Editor.EmbedLink.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Container
    | Links
    | Link
    | LinkTitle
    | LinkContent
    | Buttons
    | Button
    | ButtonInner
    | ButtonText
    | ButtonIcon


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_editor_embedLink_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
