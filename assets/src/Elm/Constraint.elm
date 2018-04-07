module Elm.Constraint
    exposing
        ( Constraint
        , check
        , decoder
        , encoder
        , fromString
        , toString
        )

{-| Helpers for working with version constraint strings in `elm.json` files.


# Constraint

@docs Constraint, check


# String Conversions

@docs toString, fromString


# JSON Conversions

@docs encode, decoder

-}

import Elm.Package.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


-- CONSTRAINT


{-| A guaranteed valid Elm constraint. That means the lower bound `v1` and
the upper bound `v2` are both valid `Elm.Version` versions, and `v1 <= v2` is
guaranteed.
-}
type Constraint
    = Constraint Version Op Op Version


type Op
    = LessThan
    | LessOrEq


{-| Check if a version is within the given constraint:

    import Elm.Version as V

    oneToTwo =
        fromString "1.0.0 <= v < 2.0.0"

    sixToTen =
        fromString "6.0.0 <= v < 10.0.0"


    -- Maybe.map (check V.one) oneToTwo == Just True
    -- Maybe.map (check V.one) sixToTen == Just False

-}
check : Version -> Constraint -> Bool
check version (Constraint lower lop uop upper) =
    checkOp lop lower version && checkOp uop version upper


checkOp : Op -> Version -> Version -> Bool
checkOp op v1 v2 =
    case Version.compare v1 v2 of
        LT ->
            True

        EQ ->
            case op of
                LessThan ->
                    False

                LessOrEq ->
                    True

        GT ->
            False



-- TO STRING


{-| Convert a `Constraint` to a `String` that works in `elm.json`
-}
toString : Constraint -> String
toString (Constraint lower lop uop upper) =
    Version.toString lower ++ opToString lop ++ "v" ++ opToString uop ++ Version.toString upper


opToString : Op -> String
opToString op =
    case op of
        LessThan ->
            " < "

        LessOrEq ->
            " <= "



-- FROM STRING


{-| Try to convert a `String` into a `Constraint`:

    fromString "1.0.0 <= v < 2.0.0"   == Just ...
    fromString "1.0.0 <= v < 10.0.0"  == Just ...
    fromString "1.0.0 <= v <= 1.0.0"  == Just ...

    fromString "1.0.0"                == Nothing
    fromString "1.0.0 <= 2.0.0"       == Nothing
    fromString "1.0.0 <=  v  < 2.0.0" == Nothing -- extra spaces
    fromString "1.0.0 <= vsn < 2.0.0" == Nothing -- not "v" only
    fromString "2.0.0 <= v < 1.0.0"   == Nothing -- unsatisfiable

-}
fromString : String -> Maybe Constraint
fromString string =
    case String.split " " string of
        [ lower, lop, "v", uop, upper ] ->
            Maybe.andThen checkConstraint <|
                Maybe.map4 Constraint
                    (Version.fromString lower)
                    (opFromString lop)
                    (opFromString uop)
                    (Version.fromString upper)

        _ ->
            Nothing


opFromString : String -> Maybe Op
opFromString op =
    case op of
        "<" ->
            Just LessThan

        "<=" ->
            Just LessOrEq

        _ ->
            Nothing


checkConstraint : Constraint -> Maybe Constraint
checkConstraint ((Constraint lower _ _ upper) as constraint) =
    case Version.compare lower upper of
        LT ->
            Just constraint

        EQ ->
            Just constraint

        GT ->
            Nothing



-- JSON


{-| Turn a `Constraint` into a string for use in `elm.json`
-}
encoder : Constraint -> Value
encoder constraint =
    Encode.string (toString constraint)


{-| Decode the constraint strings that appear in `elm.json`
-}
decoder : Decoder Constraint
decoder =
    Decode.andThen decoderHelp Decode.string


decoderHelp : String -> Decoder Constraint
decoderHelp string =
    case fromString string of
        Just constraint ->
            Decode.succeed constraint

        Nothing ->
            Decode.fail "I need a valid constraint like \"1.0.0 <= v < 2.0.0\""
