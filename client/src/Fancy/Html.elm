module Fancy.Html exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes
import Json.Encode exposing (Value)


type Property data msg
    = ComputedProperty (data -> ( data, Html.Attribute msg ))
    | KnownProperty (Html.Attribute msg)


type Style
    = Style String String


height : Int -> Style
height h =
    Style "height" (toString h ++ "px")


type Css
    = Css (Dict String (List Style))


css : List Style -> Property { a | css : Css } msg
css styles =
    ComputedProperty <|
        \data ->
            let
                (Css dict) =
                    data.css

                classHash =
                    toString styles
            in
            ( { data | css = Css <| Dict.insert classHash styles dict }
            , Html.Attributes.class classHash
            )


type Node data msg
    = Custom (data -> ( data, Html.Html msg ))


custom : (data -> ( data, Html.Html msg )) -> Node data msg
custom =
    Custom


text : String -> Node data msg
text content =
    custom <|
        \data -> ( data, Html.text content )


node : String -> List (Property data msg) -> List (Node data msg) -> Node data msg
node tagName properties children =
    custom <|
        \data ->
            let
                ( childData, childNodes ) =
                    List.foldr
                        (\(Custom run) ( currentData, childList ) ->
                            Tuple.mapSecond (\a -> a :: childList) <| run currentData
                        )
                        ( data, [] )
                        children

                ( propertyData, props ) =
                    List.foldr
                        (\property ( currentData, propList ) ->
                            case property of
                                ComputedProperty run ->
                                    Tuple.mapSecond (\a -> a :: propList) <| run currentData

                                KnownProperty attr ->
                                    ( currentData, attr :: propList )
                        )
                        ( childData, [] )
                        properties
            in
            ( propertyData
            , Html.node tagName props childNodes
            )


type Translation
    = Translation (Dict String String)


translation : String -> Node { a | translation : Translation } msg
translation key =
    custom <|
        \data ->
            let
                (Translation lookup) =
                    data.translation
            in
            ( data
            , lookup
                |> Dict.get key
                |> Maybe.withDefault key
                |> Html.text
            )


view : Node { a | css : Css, translation : Translation } msg
view =
    node "div"
        [ css
            [ height 12 ]
        ]
        [ node "span" [] [ text "hi" ]
        , node "span" [] [ translation "myTranslation" ]
        ]
