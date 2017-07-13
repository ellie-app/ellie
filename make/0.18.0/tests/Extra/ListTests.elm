module Extra.ListTests exposing (tests)

import Expect
import Extra.List as List
import Fuzz exposing (int, list, string, tuple)
import Test exposing (..)


tests : Test
tests =
    describe "Extra.List"
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
