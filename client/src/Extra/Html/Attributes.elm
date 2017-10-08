module Extra.Html.Attributes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (property)
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
    Html.Attributes.style [ ( key, value ) ]
