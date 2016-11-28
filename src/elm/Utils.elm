module Utils exposing (..)

import Regex exposing (Regex)


listFind : (a -> Bool) -> List a -> Maybe a
listFind predicate list =
    list
        |> List.filter predicate
        |> List.head


stringReplace : String -> String -> String -> String
stringReplace toReplace replaceWith source =
    Regex.replace Regex.All (Regex.regex (Regex.escape toReplace)) (\_ -> replaceWith) source
