module Shared.Utils exposing (..)

import Regex exposing (Regex)


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
