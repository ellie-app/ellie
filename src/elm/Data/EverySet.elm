module Data.EverySet exposing (..)

import EveryDict exposing (EveryDict)


type alias EverySet a =
    EveryDict a ()


fromList : List a -> EverySet a
fromList stuff =
    EveryDict.fromList <| List.map ((flip (,)) ()) stuff


member : a -> EverySet a -> Bool
member val set =
    EveryDict.member val set
