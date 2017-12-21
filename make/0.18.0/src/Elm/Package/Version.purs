module Elm.Package.Version where

import Prelude

import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, get, put)
import Data.List.NonEmpty as NonEmptyList
import Data.Read (class Read, read)
import Data.String (Pattern(..))
import Data.String as String

newtype Version =
  Version
    { major :: Int
    , minor :: Int
    , patch :: Int
    }

derive instance eqVersion :: Eq Version
derive instance ordVersion :: Ord Version

instance foreignableVersion :: Foreignable Version where
  put = show >>> put 
  get value = get value >>= (read >>> lmap (Foreign.ForeignError >>> NonEmptyList.singleton) >>> except)


instance readVersion :: Read Version where
  read input =
    case String.split (Pattern ".") input of
      [majorString, minorString, patchString] -> do
        major <- read majorString
        minor <- read minorString
        patch <- read patchString
        Right $ Version { major, minor, patch }

      _ ->
        Left "Expecting a version in the format MAJOR.MINOR.PATCH"


instance showVersion :: Show Version where
  show (Version { major, minor, patch }) =
    show major
      <> "."
      <> show minor
      <> "."
      <> show patch