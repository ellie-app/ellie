module Shared.Utils exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Regex exposing (Regex)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


filterMaybe : (a -> Bool) -> a -> Maybe a
filterMaybe predicate a =
    if predicate a then
        Just a
    else
        Nothing


listFind : (a -> Bool) -> List a -> Maybe a
listFind predicate list =
    list
        |> List.filter predicate
        |> List.head


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
