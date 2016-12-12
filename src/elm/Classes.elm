module Classes exposing (Classes(..), class, classList)

import Html exposing (Attribute)
import Html.CssHelpers


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace ""


class : List class -> Html.Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Html.Attribute msg
classList =
    helpers.classList


type Classes
    = EditorsContainer
    | EditorContainer
    | EditorsSeparator
    | WorkAreaContainer
    | ResultsEditorsSeparator
    | ResultsContainer
