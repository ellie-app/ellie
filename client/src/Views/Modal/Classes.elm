module Views.Modal.Classes
    exposing
        ( Classes(..)
        , class
        , classList
        )

import Html.CssHelpers


type Classes
    = Container
    | Backdrop
    | Content


{ class, classList } =
    Html.CssHelpers.withNamespace "Views-Modal-"
