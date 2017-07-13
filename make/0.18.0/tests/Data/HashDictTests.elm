module Data.HashDictTests exposing (tests)

import Data.HashDict as HashDict exposing (HashDict)
import Expect
import Test exposing (..)


tests : Test
tests =
    describe "Data.HashDict"
        [ test "insertWith" <|
            \() ->
                HashDict.fromList [ ( 1, [ 1 ] ), ( 2, [ 2 ] ) ]
                    |> HashDict.insertWith (flip (++)) 1 [ 3 ]
                    |> HashDict.getUnsafe 1
                    |> Expect.equal [ 1, 3 ]
        ]
