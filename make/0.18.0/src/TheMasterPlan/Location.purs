module TheMasterPlan.Location where

import Ellie.Prelude

import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, put, get)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype)
import Elm.Package (Package)
import System.FileSystem (FilePath)


newtype Location =
  Location
    { relativePath :: FilePath
    , package :: Package
    }

derive instance eqLocation :: Eq Location
derive instance ordLocation :: Ord Location
derive instance newtypeLocation :: Newtype Location _


instance foreignableLocation :: Foreignable Location where
  put (Location { relativePath, package }) =
    Foreign.toForeign { relativePath, package: put package}

  get value =
    { relativePath: _, package: _ }
      <$> (value ! "relativePath" >>= get)
      <*> (value ! "package" >>= get)
      <#> Location

