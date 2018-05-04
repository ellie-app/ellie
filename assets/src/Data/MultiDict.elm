module Data.MultiDict exposing (MultiDict, empty, get, insert, remove, update)

import Dict exposing (Dict)


type MultiDict k v
    = MultiDict (Dict k (List v))


insert : comparable -> v -> MultiDict comparable v -> MultiDict comparable v
insert key value (MultiDict dict) =
    MultiDict <|
        Dict.update
            key
            (\maybeStuff ->
                case maybeStuff of
                    Just stuff ->
                        Just (value :: stuff)

                    Nothing ->
                        Just [ value ]
            )
            dict


remove : comparable -> MultiDict comparable v -> MultiDict comparable v
remove key (MultiDict dict) =
    MultiDict
        (Dict.remove key dict)


empty : MultiDict k v
empty =
    MultiDict Dict.empty


update : comparable -> (List v -> List v) -> MultiDict comparable v -> MultiDict comparable v
update key fn (MultiDict dict) =
    MultiDict <|
        Dict.update
            key
            (\maybeStuff ->
                let
                    updated =
                        case maybeStuff of
                            Just stuff ->
                                fn stuff

                            Nothing ->
                                fn []
                in
                case updated of
                    [] ->
                        Nothing

                    stuff ->
                        Just stuff
            )
            dict


get : comparable -> MultiDict comparable v -> List v
get key (MultiDict dict) =
    case Dict.get key dict of
        Just stuff ->
            stuff

        Nothing ->
            []
