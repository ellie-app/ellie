module Data.Elm.Package.Constraint exposing (Constraint, check, decoder, encoder, fromString, fromVersion, isSatisfied, toString)

import Data.Elm.Package.Version as Version exposing (Version)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Op
    = Less
    | LessOrEqual


type Constraint
    = Range Version Op Op Version


opToString : Op -> String
opToString op =
    case op of
        Less ->
            "<"

        LessOrEqual ->
            "<="


opFromString : String -> Maybe Op
opFromString str =
    case str of
        "<" ->
            Just Less

        "<=" ->
            Just LessOrEqual

        _ ->
            Nothing


fromString : String -> Maybe Constraint
fromString str =
    case String.split " " str of
        [ firstVersion, firstOp, "v", lastOp, lastVersion ] ->
            Maybe.map4
                Range
                (Version.fromString firstVersion)
                (opFromString firstOp)
                (opFromString lastOp)
                (Version.fromString lastVersion)

        _ ->
            Nothing


isSatisfied : Constraint -> Version -> Bool
isSatisfied constraint version =
    case constraint of
        Range lower lowerOp upperOp upper ->
            isLess lowerOp lower version && isLess upperOp version upper


check : Constraint -> Version -> Order
check constraint version =
    case constraint of
        Range lower lowerOp upperOp upper ->
            if isLess lowerOp lower version then
                LT
            else if isLess upperOp version upper then
                GT
            else
                EQ


isLess : Op -> Version -> Version -> Bool
isLess op left right =
    case op of
        Less ->
            Version.compare left right == LT

        LessOrEqual ->
            Version.compare left right /= GT


toString : Constraint -> String
toString constraint =
    case constraint of
        Range lower lowerOp upperOp upper ->
            Version.toString lower
                ++ " "
                ++ opToString lowerOp
                ++ " v "
                ++ opToString upperOp
                ++ " "
                ++ Version.toString upper


decoder : Decode.Decoder Constraint
decoder =
    let
        check str =
            case fromString str of
                Just constraint ->
                    Decode.succeed constraint

                Nothing ->
                    Decode.fail "a constraint like \"1.0.0 <= v < 2.0.0\""
    in
    Decode.andThen check Decode.string


encoder : Constraint -> Value
encoder constraint =
    Encode.string <| toString constraint


fromVersion : Version -> Constraint
fromVersion version =
    Range
        version
        LessOrEqual
        Less
        (Version (version.major + 1) 0 0)
