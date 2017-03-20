module Components.Button.View
    exposing
        ( view
        )

import Html exposing (Html, a, p, div, text, button, span, img)
import Html.Attributes exposing (style, src, href, id, target)
import Shared.Icons as Icons
import Shared.Colors as Colors
import Shared.Constants as Constants
import Components.About.Classes exposing (..)


fullWidth : Html msg
fullWidth =
