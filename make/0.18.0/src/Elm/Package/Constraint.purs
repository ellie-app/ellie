module Elm.Package.Constraint
    ( Constraint
    , isSatisfied
    , check
    , forVersion
    ) where

import Ellie.Prelude

import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, get, put)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Read (class Read, read)
import Data.String as String
import Data.Tuple (Tuple(..))
import Elm.Package.Version (Version(..))


break :: (Char -> Boolean) -> String -> Tuple String String
break breakOn input =
  Tuple
    (String.takeWhile (breakOn >>> not) input)
    (String.dropWhile (breakOn >>> not) input)


data Op
  = Less
  | LessOrEqual

instance showOp :: Show Op where
  show op =
    case op of
      Less -> "<"
      LessOrEqual -> "<="

data Constraint
    = Range Version Op Op Version


instance foreignableConstraing :: Foreignable Constraint where
  put = show >>> put
  get value = get value >>= (read >>> lmap (Foreign.ForeignError >>> NonEmptyList.singleton) >>> except)


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
      (Tuple lowerOp rest1) <- takeOp (chompSpace rest)
      rest2 <- chompV (chompSpace rest1)
      (Tuple upperOp rest3) <- takeOp (chompSpace rest2)
      upper <- read (chompSpace rest3)
      pure (Range lower lowerOp upperOp upper)
    where
      takeOp :: String -> Either String (Tuple Op String)
      takeOp str =
        case String.uncons str of
          Just { head: '<', tail: rest1 } ->
            case String.uncons rest1 of
              Just { head: '=', tail: rest } -> Right $ Tuple LessOrEqual rest
              _ -> Right $ Tuple Less rest1
          _ ->
            Left "Expecting a constraint in the format 1.0.0 <= v < 2.0.0"

      chompSpace :: String -> String
      chompSpace str =
        case String.uncons str of
          Just { head: ' ', tail } -> tail
          _ -> str

      chompV :: String -> Either String String
      chompV str =
        case String.uncons str of
          Just { head: 'v', tail } -> Right tail
          _ -> Left "Expecting a constraint in the format 1.0.0 <= v < 2.0.0"



forVersion :: Version -> Constraint
forVersion version@(Version v) =
  Range
    version
    LessOrEqual
    Less
    (Version <| v { patch = v.patch + 1 })



isSatisfied :: Constraint -> Version -> Boolean
isSatisfied constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
        isLess lowerOp lower version
          &&
        isLess upperOp version upper


isLess :: ∀ a. (Ord a) => Op -> (a -> a -> Boolean)
isLess op =
  case op of
    Less ->
      (<)

    LessOrEqual ->
      (<=)


check :: Constraint -> Version -> Ordering
check constraint version =
  case constraint of
    Range lower lowerOp upperOp upper ->
      if not (isLess lowerOp lower version) then
        LT

      else if not (isLess upperOp version upper) then
        GT

      else
        EQ
