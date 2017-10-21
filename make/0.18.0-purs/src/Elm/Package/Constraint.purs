module Elm.Package.Constraint
    ( Constraint
    , isSatisfied
    , check
    ) where

import Prelude
import Data.String.Read (class Read)
import Elm.Package as Package
import Elm.Compiler as Compiler
import Data.String as String
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))


break :: (Char -> Boolean) -> String -> Tuple String String
break breakOn input =
  Tuple
    (String.takeWhile (breakOn >>> not) input)
    (String.dropWhile (breakOn >>> not) input)


-- CONSTRAINTS


data Op
  = Less
  | LessOrEqual


instance showOp :: Show Op where
  show op =
    case op of
      Less -> "<"
      LessOrEqual -> "<="


data Constraint
    = Range Package.Version Op Op Package.Version


instance showConstraint :: Show Constraint where
  show constraint =
    case constraint of
      Range lower lowerOp upperOp upper ->
        String.joinWith " "
          [ show lower
          , show lowerOp
          , "v"
          , show upperOp
          , show upper
          ]


instance readConstraint :: Read Constraint where
  read input =
    do
      let (Tuple lowerString rest) = break (_ == ' ') input
      lower <- read lowerString
      (Tuple lowerOp rest1) <- takeOp (eatSpace rest)
      rest2 <- chompV (chompSpace rest1)
      (Tuple upperOp rest3) <- takeOp (chompSpace rest2)
      upper <- read (chompSpace rest3)
      pure (Range lower lowerOp upperOp upper)
    where
      takeOp :: String -> Maybe (Tuple Op String)
      takeOp str =
        case String.uncons str of
          Just { head: '<', tail: rest1 } ->
            case String.uncons rest1 of
              Just { head: '=', tail: rest } -> Just $ Tuple LessOrEqual rest
              Nothing -> Just $ Tuple Less rest1
          _ ->
            Nothing

      chompSpace :: String -> String
      chompSpace str =
        case String.uncons str of
          Just { head: ' ', tail } -> tail
          _ -> str

      chompV :: String -> Maybe String
      chompV str =
        case String.uncons str of
          Just { head: 'v', tail } -> Just tail
          _ -> Nothing


-- CHECK IF SATISFIED


isSatisfied :: Constraint -> Package.Version -> Boolean
isSatisfied constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
        isLess lowerOp lower version
          &&
        isLess upperOp version upper


isLess :: (Ord a) => Op -> (a -> a -> Boolean)
isLess op =
  case op of
    Less ->
      (<)

    LessOrEqual ->
      (<=)


check :: Constraint -> Package.Version -> Ordering
check constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
      if not (isLess lowerOp lower version) then
        LT

      else if not (isLess upperOp version upper) then
        GT

      else
        EQ
