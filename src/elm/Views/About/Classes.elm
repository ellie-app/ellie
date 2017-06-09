module Views.About.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Popout
    | Title
    | Paragraph
    | Link
    | Creators
    | CreatorLine
    | Logo
    | ImagesAndHsLogo
    | PartnerImageContainer
    | PartnerImage
    | HsLogoImage


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace "components_about_"


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
