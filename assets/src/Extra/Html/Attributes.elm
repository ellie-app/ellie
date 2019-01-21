module Extra.Html.Attributes exposing (cond, maybe, none, style)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes exposing (property)
import Json.Encode as Encode


cond : Attribute msg -> Bool -> Attribute msg
cond attr bool =
    if bool then
        attr

    else
        none


maybe : Maybe (Attribute msg) -> Attribute msg
maybe =
    Maybe.withDefault none


none : Attribute msg
none =
    property "" Encode.null


style : String -> String -> Attribute msg
style key value =
    Attributes.style key value
