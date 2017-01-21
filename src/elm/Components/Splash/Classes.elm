module Components.Splash.Classes exposing (..)

import Html exposing (Attribute)
import Html.CssHelpers


type Classes
    = Container
    | MainText
    | SubText


namespace : String
namespace =
    "components_splash_"


helpers : Html.CssHelpers.Namespace String class id msg
helpers =
    Html.CssHelpers.withNamespace namespace


class : List class -> Attribute msg
class =
    helpers.class


classList : List ( class, Bool ) -> Attribute msg
classList =
    helpers.classList
