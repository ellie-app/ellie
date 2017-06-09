module Data.Extra.ListTests exposing (tests)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import Data.Extra.List as List


tests : Test
tests =
    describe "Data.Extra.List"
        [ fuzz (list int) "mapAccumR" <|
            \inputInts ->
                List.mapAccumR
                    (\next memo -> ( memo + next, next + 1 ))
                    0
                    inputInts
                    |> Expect.all
                        [ Tuple.first >> Expect.equal (List.sum inputInts)
                        , Tuple.second >> Expect.equal (List.map ((+) 1) inputInts)
                        ]
        , fuzz (list int) "foldl1" <|
            \inputInts ->
                inputInts
                    |> List.foldl1 (+)
                    |> Expect.equal
                        (if List.length inputInts == 0 then
                            Nothing
                         else
                            Just (List.sum inputInts)
                        )
        ]
