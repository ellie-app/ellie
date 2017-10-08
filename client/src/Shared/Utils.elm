module Shared.Utils exposing (..)

import Set exposing (Set)


filterMaybe : (a -> Bool) -> a -> Maybe a
filterMaybe predicate a =
    if predicate a then
        Just a
    else
        Nothing


hashFilter : (b -> String) -> (a -> String) -> List b -> List a -> List a
hashFilter hashEx hashM excluded values =
    let
        set =
            excluded
                |> List.map hashEx
                |> Set.fromList
    in
    List.filter (\a -> not (Set.member (hashM a) set)) values


numberToPercent : number -> String
numberToPercent number =
    toString (number * 100) ++ "%"


boolToMaybe : Bool -> Maybe ()
boolToMaybe bool =
    if bool then
        Just ()
    else
        Nothing
