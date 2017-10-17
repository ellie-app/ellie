module Elm.Compiler.Module
    ( Interface
    , nameToPath
    , nameToString
    , nameFromString
    , hyphenate
    , dehyphenate
    , module ModuleName
    )
  where

import Prelude (($))
import Data.Foldable (foldl)
import System.FilePath (FilePath, (</>))
import Elm.Compiler.Module.Name (Canonical(..), Raw(..)) as ModuleName
import Data.String (Pattern(..), joinWith, split)
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)


-- INTERFACES


newtype Interface = Interface String

derive instance genericInterface :: Generic Interface
instance decodeJsonInterface :: DecodeJson Interface where decodeJson = gDecodeJson
instance encodeJsonInterface :: EncodeJson Interface where encodeJson = gEncodeJson


-- STRING CONVERSIONS for RAW NAMES


nameToPath :: ModuleName.Raw -> FilePath
nameToPath (ModuleName.Raw segments) =
  foldl (</>) "" segments


nameToString :: ModuleName.Raw -> String
nameToString (ModuleName.Raw segments) =
  joinWith "." segments


nameFromString :: String -> ModuleName.Raw
nameFromString input =
  ModuleName.Raw $ split (Pattern ".") input


hyphenate :: ModuleName.Raw -> String
hyphenate (ModuleName.Raw segments) =
  joinWith "-" segments


dehyphenate :: String -> ModuleName.Raw
dehyphenate input =
  ModuleName.Raw $ split (Pattern "-") input
