module Pages.Embed.Classes exposing (Classes(..), class, classList)

import Html exposing (Attribute)
import Html.CssHelpers


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "app_"


class : List class -> Html.Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Html.Attribute msg
classList =
    helpers.classList


type Classes
    = Container
    | LoadingContainer
    | FailureContainer
    | FailureTitle
    | FailureMessage
    | FailureDetails
    | Header
    | HeaderLink
    | HeaderLinkInner
    | HeaderLinkIcon
    | HeaderLeft
    | HeaderRight
    | HeaderTab
    | HeaderTabInner
    | HeaderTabActive
    | HeaderTabIcon
    | HeaderLinkLogo
    | WorkArea
    | LoadedContainer
    | WorkAreaTab
    | WorkAreaTabHidden
    | Iframe
