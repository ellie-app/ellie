module Elm.Compiler.Module.Name.Raw where

import Ellie.Prelude

import Control.Monad.Except (except)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable)
import Data.Foreign.Class as Foreign
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Read (read, class Read)
import Data.String (Pattern(..))
import Data.String as String
import System.FileSystem (FilePath, (</>))

newtype Raw =
  Raw (Array String)

derive instance eqRaw :: Eq Raw
derive instance ordRaw :: Ord Raw
derive instance newtypeRaw :: Newtype Raw _

instance foreignableRaw :: Foreignable Raw where
  put = show >>> Foreign.put
  get value = Foreign.get value >>= (read >>> lmap (Foreign.ForeignError >>> NonEmptyList.singleton) >>> except)

instance showRaw :: Show Raw where
  show (Raw segments) =
    String.joinWith "." segments

instance readRaw :: Read Raw where
  read input =
    Right $ Raw $ String.split (Pattern ".") input


isNative :: Raw -> Boolean
isNative (Raw array) =
  Array.head array == Just "Native"

toPath :: Raw -> FilePath
toPath (Raw segments) =
  Array.foldl (</>) "" segments

hyphenate :: Raw -> String
hyphenate (Raw segments) =
  String.joinWith "-" segments

dehyphenate :: String -> Raw
dehyphenate input =
  Raw $ String.split (Pattern "-") input
