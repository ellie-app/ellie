module Ellie.Elm.Compiler.Error
  ( Error
  , Location
  , Region
  ) where

import Prelude

import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Foreign.Index ((!))
import Data.Foreign.NullOrUndefined (readNullOrUndefined, unNullOrUndefined) as Foreign
import Data.Maybe (Maybe)
import Data.Nullable as Nullable

newtype Location =
  Location
    { line :: Int
    , column :: Int
    }

instance decodeLocation :: Decode Location where
  decode object =
    { line: _, column: _ }
      <$> (object ! "line" >>= Foreign.decode)
      <*> (object ! "column" >>= Foreign.decode)
      <#> Location

instance encodeLocation :: Encode Location where
  encode (Location location) =
    Foreign.toForeign location


newtype Region =
  Region
    { start :: Location
    , end :: Location
    }

instance decodeRegion :: Decode Region where
  decode object =
    { start: _, end: _ }
      <$> (object ! "start" >>= Foreign.decode)
      <*> (object ! "end" >>= Foreign.decode)
      <#> Region

instance encodeRegion :: Encode Region where
  encode (Region { start, end }) =
    Foreign.toForeign
      { start: Foreign.encode start
      , end: Foreign.encode end
      }


newtype Error =
  Error
    { tag :: String
    , overview :: String
    , details :: String
    , subregion :: Maybe Region
    , region :: Region
    , level :: String
    }

instance decodeError :: Decode Error where
  decode object =
    { tag: _, overview: _, details: _, subregion: _, region: _, level: _ }
      <$> (object ! "tag" >>= Foreign.decode)
      <*> (object ! "overview" >>= Foreign.decode)
      <*> (object ! "details" >>= Foreign.decode)
      <*> (object ! "subregion" >>= Foreign.readNullOrUndefined Foreign.decode <#> Foreign.unNullOrUndefined)
      <*> (object ! "region" >>= Foreign.decode)
      <*> (object ! "level" >>= Foreign.decode)
      <#> Error

instance encodeError :: Encode Error where
  encode (Error error) =
    Foreign.toForeign
      { tag: error.tag
      , overview: error.overview
      , details: error.details
      , subregion: Nullable.toNullable $ map Foreign.encode error.subregion
      , region: Foreign.encode error.region
      , level: error.level
      }
