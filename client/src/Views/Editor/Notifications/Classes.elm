module Views.Editor.Notifications.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Notifications
    | Button
    | Popout
    | PopoutHidden
    | Item
    | ItemIcon
    | ItemDetails
    | Items
    | ItemTitle
    | ItemTimestamp
    | ItemMessage
    | Latest
    | LatestTitle
    | LatestIcon
    | ItemHighlighted
    | ButtonIcon


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_notifications_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
