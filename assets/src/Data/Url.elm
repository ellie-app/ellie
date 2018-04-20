module Data.Url
    exposing
        ( Url
        , fromString
        , href
        , src
        , toString
        )

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes


type Url
    = Url String


fromString : String -> Url
fromString =
    Url


toString : Url -> String
toString (Url string) =
    string


src : Url -> Attribute msg
src (Url string) =
    Attributes.src string


href : Url -> Attribute msg
href (Url string) =
    Attributes.src string
