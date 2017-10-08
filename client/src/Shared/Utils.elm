module Shared.Utils exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Regex exposing (Regex)
import Set exposing (Set)


filterMaybe : (a -> Bool) -> a -> Maybe a
filterMaybe predicate a =
    if predicate a then
        Just a
    else
        Nothing


stringReplace : String -> String -> String -> String
stringReplace toReplace replaceWith source =
    Regex.replace Regex.All (Regex.regex (Regex.escape toReplace)) (\_ -> replaceWith) source


mapCmd : (a -> b) -> ( model, Cmd a ) -> ( model, Cmd b )
mapCmd tagger ( model, cmd ) =
    ( model, Cmd.map tagger cmd )


mapModel : (a -> b) -> ( a, Cmd msg ) -> ( b, Cmd msg )
mapModel tagger ( model, cmd ) =
    ( tagger model, cmd )


addCmds : List (Cmd a) -> ( model, Cmd a ) -> ( model, Cmd a )
addCmds moreCmds ( model, cmd ) =
    ( model, Cmd.batch (cmd :: moreCmds) )


customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder start parse =
    Decode.andThen
        (\input ->
            case parse input of
                Ok thing ->
                    Decode.succeed thing

                Err message ->
                    Decode.fail message
        )
        start


encodeNullable : (a -> Value) -> Maybe a -> Value
encodeNullable encoder maybe =
    maybe
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null


hashFilter : (b -> String) -> (a -> String) -> List b -> List a -> List a
hashFilter hashEx hashM excluded values =
    let
        set =
            excluded
                |> List.map hashEx
                |> Set.fromList
    in
    List.filter (\a -> not (Set.member (hashM a) set)) values


decodeUnion : String -> List ( String, Decoder a ) -> Decoder a
decodeUnion topTag list =
    let
        decoderDict =
            list
                |> List.map (\( l, r ) -> ( topTag ++ "." ++ l, r ))
                |> Dict.fromList
    in
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                Dict.get tag decoderDict
                    |> Maybe.withDefault (Decode.fail <| "Could not decode value with tag " ++ tag ++ " for type " ++ topTag)
            )


numberToPercent : number -> String
numberToPercent number =
    toString (number * 100) ++ "%"


resultIsSuccess : Result x a -> Bool
resultIsSuccess result =
    result
        |> Result.map (\_ -> True)
        |> Result.withDefault False


boolToMaybe : Bool -> Maybe ()
boolToMaybe bool =
    if bool then
        Just ()
    else
        Nothing


clamp : comparable -> comparable -> comparable -> comparable
clamp minimum maximum current =
    if current >= maximum then
        maximum
    else if current <= minimum then
        minimum
    else
        current


innerHtml : String -> Html.Attribute msg
innerHtml string =
    Html.Attributes.property "innerHTML" <| Encode.string string


replaceBackticks : String -> String
replaceBackticks string =
    Regex.replace
        Regex.All
        (Regex.regex "`([^`]+)`")
        (\match ->
            match.submatches
                |> List.head
                |> Maybe.andThen identity
                |> Maybe.map (\submatch -> "<code>" ++ submatch ++ "</code>")
                |> Maybe.withDefault match.match
        )
        string


replaceNewlines : String -> String
replaceNewlines string =
    Regex.replace
        Regex.All
        (Regex.regex "\n")
        (\_ -> "<br />")
        string


replaceAll : String -> String
replaceAll string =
    string
        |> replaceBackticks
        |> replaceNewlines


renderIf : Bool -> (() -> Html msg) -> Html msg
renderIf predicate thunk =
    if predicate then
        thunk ()
    else
        Html.text ""
