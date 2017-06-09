module Data.Extra.EveryDictTests exposing (tests)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import EveryDict exposing (EveryDict)
import Data.Extra.EveryDict as EveryDict


tests : Test
tests =
    describe "Data.Extra.EveryDict"
        [ test "insertWith" <|
            \() ->
                EveryDict.fromList [ ( 1, [ 1 ] ), ( 2, [ 2 ] ) ]
                    |> EveryDict.insertWith (flip (++)) 1 [ 3 ]
                    |> EveryDict.getUnsafe 1
                    |> Expect.equal [ 1, 3 ]
        ]
