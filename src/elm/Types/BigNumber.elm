module Types.BigNumber
    exposing
        ( BigNumber
        , fromString
        , fromFloat
        , fromInt
        , add
        , subtract
        , multiply
        , divide
        , floor
        , toFloat
        , toString
        , toInt
        , zero
        , (|+|)
        , (|-|)
        , (|*|)
        , (|/|)
        , (|>|)
        , (|%|)
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


infixl 6 |+|
(|+|) : BigNumber -> number -> BigNumber
(|+|) =
    Native.BigNumber.add


subtract : number -> BigNumber -> BigNumber
subtract number bignum =
    Native.BigNumber.sub bignum number


infixl 6 |-|
(|-|) : BigNumber -> number -> BigNumber
(|-|) =
    Native.BigNumber.sub


multiply : number -> BigNumber -> BigNumber
multiply number bignum =
    Native.BigNumber.mul bignum number


infixl 7 |*|
(|*|) : BigNumber -> number -> BigNumber
(|*|) =
    Native.BigNumber.mul


divide : number -> BigNumber -> BigNumber
divide number bignum =
    Native.BigNumber.div bignum number


infixl 7 |/|
(|/|) : BigNumber -> number -> BigNumber
(|/|) =
    Native.BigNumber.div


infix 4 |>|
(|>|) : BigNumber -> number -> Bool
(|>|) =
    Native.BigNumber.gt


infixl 7 |%|
(|%|) : BigNumber -> number -> BigNumber
(|%|) =
    Native.BigNumber.mod


floor : BigNumber -> BigNumber
floor =
    Native.BigNumber.floor


toInt : BigNumber -> Int
toInt =
    toFloat >> round


toFloat : BigNumber -> Float
toFloat =
    Native.BigNumber.toFloat


toString : BigNumber -> String
toString =
    Native.BigNumber.toString
