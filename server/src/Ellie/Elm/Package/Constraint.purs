module Ellie.Elm.Package.Constraint
    ( Constraint
    , isSatisfied
    , lowestVersion
    ) where

import Prelude

import Control.Monad.Except as Except
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (F)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class as Foreign
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Ellie.Elm.Package.Version (Version(..))


break :: (Char -> Boolean) -> String -> Tuple String String
break breakOn input =
  Tuple
    (String.takeWhile (breakOn >>> not) input)
    (String.dropWhile (breakOn >>> not) input)


data Op
  = Less
  | LessOrEqual

opToString :: Op -> String
opToString Less = "<"
opToString LessOrEqual = "<="


data Constraint
    = Range Version Op Op Version

instance showConstraint :: Show Constraint where
  show (Range v o o' v') = show v <> " " <> opToString o <> " v " <> opToString o' <> " " <> show v'

instance encodeConstraint :: Encode Constraint where
  encode constraint = do
    case constraint of
      Range (Version lower) lowerOp upperOp (Version upper) ->
        Foreign.encode $
          String.joinWith " "
            [ show lower.major <> "." <> show lower.minor <> "." <> show lower.patch
            , opToString lowerOp
            , "v"
            , opToString upperOp
            , show upper.major <> "." <> show upper.minor <> "." <> show upper.patch
            ]

instance decodeConstraint :: Decode Constraint where
  decode input = do
    string ← Foreign.decode input
    case String.split (Pattern " ") string of
      [lower, lowerOp, "v", upperOp, upper] →
        Range
          <$> (Foreign.decode (Foreign.toForeign lower))
          <*> takeOp lowerOp
          <*> takeOp upperOp
          <*> (Foreign.decode (Foreign.toForeign upper))
      _ →
        Foreign.fail $ Foreign.ForeignError "Expecting a constraint in the format 1.0.0 <= v < 2.0.0"

    where
      takeOp :: String -> F Op
      takeOp "<" = pure Less
      takeOp "<=" = pure LessOrEqual
      takeOp _ = Foreign.fail $ Foreign.ForeignError "Expecting a constraint in the format 1.0.0 <= v < 2.0.0"


lowestVersion ∷ Constraint → Version
lowestVersion (Range v _ _ _) = v

forVersion :: Version -> Constraint
forVersion version@(Version v) =
  Range
    version
    LessOrEqual
    Less
    (Version $ v { patch = v.patch + 1 })



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