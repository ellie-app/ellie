module Types.BigNumber
    exposing
        ( fromString
        , fromFloat
        , fromInt
        , add
        , sub
        , mul
        , div
        , floor
        , toFloat
        , toInt
        , zero
        , (|+|)
        , (|-|)
        , (|*|)
        , (|/|)
        )

import Native.BigNumber


type BigNumber
    = BigNumber


fromString : String -> Result String BigNumber
fromString =
    Native.BigNumber.create


fromInt : Int -> Result String BigNumber
fromInt =
    Native.BigNumber.create


fromFloat : Float -> Result String BigNumber
fromFloat =
    Native.BigNumber.create


zero : BigNumber
zero =
    Native.BigNumber.zero


add : number -> BigNumber -> BigNumber
add number bignum =
    Native.BigNumber.add bignum number


(|+|) : BigNumber -> number -> BigNumber
(|+|) =
    Native.BigNumber.add


sub : number -> BigNumber -> BigNumber
sub number bignum =
    Native.BigNumber.sub bignum number


(|-|) : BigNumber -> number -> BigNumber
(|-|) =
    Native.BigNumber.sub


mul : number -> BigNumber -> BigNumber
mul number bignum =
    Native.BigNumber.mul bignum number


(|*|) : BigNumber -> number -> BigNumber
(|*|) =
    Native.BigNumber.mul


div : number -> BigNumber -> BigNumber
div number bignum =
    Native.BigNumber.div bignum number


(|/|) : BigNumber -> number -> BigNumber
(|/|) =
    Native.BigNumber.div


floor : BigNumber -> BigNumber
floor =
    Native.BigNumber.floor


toInt : BigNumber -> Int
toInt =
    Native.BigNumber.toInt


toFloat : BigNumber -> Float
toFloat =
    Native.BigNumber.toFloat
