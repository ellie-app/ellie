module Data.HashSet exposing (..)

import IntDict exposing (IntDict)
import Native.Hash


hash : a -> Int
hash =
    Native.Hash.hash


type HashSet a
    = HashSet (IntDict a)


fromList : List a -> HashSet a
fromList stuff =
    HashSet
        (stuff |> List.map (\a -> ( hash a, a )) |> IntDict.fromList)


member : a -> HashSet a -> Bool
member val (HashSet dict) =
    IntDict.member (hash val) dict
