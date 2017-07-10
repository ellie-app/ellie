module Views.Output.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Iframe
    | Overlay
    | OverlayTitle
    | OverlaySubtitle
    | OverlayContent
    | ErrorsContainer
    | ErrorItem
    | ErrorItemName
    | ErrorItemOverview
    | ErrorItemDetails
    | ErrorItemLocation
    | ErrorItemHeader
    | ManyModulesWarning


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_output_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
