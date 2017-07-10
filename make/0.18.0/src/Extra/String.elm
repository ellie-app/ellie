module Extra.String exposing (..)

import Bitwise
import Char
import Regex exposing (Regex)


replace : String -> String -> String -> String
replace pattern replacement input =
    Regex.replace
        Regex.All
        (Regex.regex pattern)
        (\_ -> replacement)
        input


hash : String -> Int
hash input =
    case String.uncons input of
        Nothing ->
            0

        Just ( head, rest ) ->
            input
                |> String.foldr
                    (\char hashValue -> Bitwise.xor (Char.toCode char) (hashValue * 1000003))
                    (Bitwise.shiftLeftBy 7 (Char.toCode head))
                |> Bitwise.xor (String.length input)
