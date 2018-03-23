module Ellie.Ui.Markdown exposing (..)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Json.Encode as Encode


view : String -> Html msg
view value =
    Html.node "ellie-ui-markdown"
        [ Attributes.property "markdownValue" <| Encode.string value ]
        []
