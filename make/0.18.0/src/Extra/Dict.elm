module Extra.Dict exposing (..)

import Dict exposing (Dict)


unionWith : (a -> a -> a) -> Dict comparable a -> Dict comparable a -> Dict comparable a
unionWith f left right =
    Dict.merge
        (\k v out -> Dict.insert k v out)
        (\k l r out -> Dict.insert k (f l r) out)
        (\k v out -> Dict.insert k v out)
        left
        right
        Dict.empty


unionsWith : (a -> a -> a) -> List (Dict comparable a) -> Dict comparable a
unionsWith f ts =
    List.foldl (unionWith f) Dict.empty ts


mapKeys : (comparable -> comparable_) -> Dict comparable v -> Dict comparable_ v
mapKeys f dict =
    dict
        |> Dict.toList
        |> List.map (\( k, v ) -> ( f k, v ))
        |> Dict.fromList
