module Elm.Compiler.Error
  ( Location
  , Region
  , Error
  ) where

import Ellie.Prelude

import Data.Foreign (Foreign, F)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, put, get)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)


type Location =
  { line :: Int
  , column :: Int
  }


type Region =
  { start :: Location
  , end :: Location
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


instance foreignableError :: Foreignable Error where
  put (Error value) =
    Foreign.toForeign <|
      { tag: put value.tag
      , overview: put value.overview
      , details: put value.details
      , subregion: put <| map putRegion value.subregion
      , region: putRegion value.region
      , level: put value.level
      }

  get value =
    { tag: _, overview: _, details: _, subregion: _, region: _, level: _ }
      <$> (value ! "tag" >>= get)
      <*> (value ! "overview" >>= get)
      <*> (value ! "details" >>= get)
      <*> (value ! "subregion" >>= Foreign.readNull >>= traverse getRegion)
      <*> (value ! "region" >>= getRegion)
      <*> (value ! "level" >>= get)
      <#> Error


getLocation :: Foreign -> F Location
getLocation value =
  { line: _, column: _ }
    <$> (value ! "line" >>= get)
    <*> (value ! "column" >>= get)

putLocation :: Location -> Foreign
putLocation location =
  Foreign.toForeign
    { line: put location.line
    , column: put location.column
    }


putRegion :: Region -> Foreign
putRegion region  =
  Foreign.toForeign
    { start: putLocation region.start
    , end: putLocation region.end
    }

getRegion :: Foreign -> F Region
getRegion value =
  { start: _, end: _ }
    <$> (value ! "start" >>= getLocation)
    <*> (value ! "end" >>= getLocation)