module Extra.String exposing (..)

import Regex exposing (Regex)


replace : String -> String -> String -> String
replace pattern replacement input =
    Regex.replace
        Regex.All
        (Regex.regex pattern)
        (\_ -> replacement)
        input


fromInt : Int -> String
fromInt int =
    toString int


fromFloat : Float -> String
fromFloat float =
    toString float
