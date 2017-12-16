module Elm.Compiler.Module.Name.Raw where

import Ellie.Prelude
import Data.String (Pattern(..))
import Data.String as String
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Read (class Read)
import Data.Either (Either(..))
import System.FileSystem (FilePath, (</>))
import Data.Foreign.Class (class Foreignable)
import Data.Foreign.Class as Foreign

newtype Raw =
  Raw (Array String)

derive instance eqRaw :: Eq Raw
derive instance ordRaw :: Ord Raw
derive instance newtypeRaw :: Newtype Raw _

instance foreignableRaw :: Foreignable Raw where
  put (Raw names) = Foreign.put names
  get value = map Raw $ Foreign.get value 


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
