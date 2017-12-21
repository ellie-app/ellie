module TheMasterPlan.Package where

import Ellie.Prelude
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Elm.Compiler.Module.Name.Raw (Raw)
import Elm.Package (Package)
import System.FileSystem (FilePath)


newtype PackageGraph =
  PackageGraph
    { data :: Map Raw PackageData
    , natives :: Map Raw FilePath
    , foreignDependencies :: Map Raw Package
    }

derive instance eqPackageGraph :: Eq PackageGraph
derive instance ordPackageGraph :: Ord PackageGraph
derive instance newtypePackageGraph :: Newtype PackageGraph _


newtype PackageData =
  PackageData
    { path :: FilePath
    , dependencies :: Array Raw
    }

derive instance eqPackageData :: Eq PackageData
derive instance ordPackageData :: Ord PackageData
derive instance newtypePackageData :: Newtype PackageData _
