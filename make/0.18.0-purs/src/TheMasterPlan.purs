module TheMasterPlan where

import Prelude (class Eq, class Ord)
import Data.List (List)
import Data.Map (Map)
import Elm.Compiler.Module (Canonical(..), Raw, Interface) as Module
import Elm.Package (Name, Version) as Package
import System.FilePath (FilePath)
import Data.Generic (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)

-- UNIQUE IDENTIFIERS FOR MODULES

newtype CanonicalModule =
  CanonicalModule
    { package :: Package
    , name :: Module.Raw
    }

derive instance eqCanonicalModule :: Eq CanonicalModule
derive instance ordCanonicalModule :: Ord CanonicalModule
derive instance genericCanonicalModule :: Generic CanonicalModule
instance decodeJsonCanonicalModule :: DecodeJson CanonicalModule where decodeJson = gDecodeJson
instance encodeJsonCanonicalModule :: EncodeJson CanonicalModule where encodeJson = gEncodeJson



simplifyModuleName :: CanonicalModule -> Module.Canonical
simplifyModuleName (CanonicalModule { package: (Package { package }), name }) =
  Module.Canonical { package: package, modul: name }


newtype Package =
  Package
    { package :: Package.Name
    , version :: Package.Version
    }

derive instance eqPackage :: Eq Package
derive instance ordPackage :: Ord Package
derive instance genericPackage :: Generic Package


-- CRAWL AN INDIVIDUAL PACKGE


newtype PackageGraph =
  PackageGraph
    { data :: Map Module.Raw PackageData
    , natives :: Map Module.Raw FilePath
    , foreignDependencies :: Map Module.Raw Package
    }


newtype PackageData =
  PackageData
    { path :: FilePath
    , depenencies :: List Module.Raw
    }



-- COMBINE ALL PACKAGE SUMMARIES


{-| Very similar to a PackageGraph, but we now have made each module name
unique by adding which package it comes from. This makes it safe to merge a
bunch of PackageGraphs together, so we can write the rest of our code
without thinking about package boundaries.
-}
newtype ProjectGraph a =
  ProjectGraph
    { data :: Map CanonicalModule (ProjectData a)
    , natives :: Map CanonicalModule Location
    }


newtype ProjectData a =
  ProjectData
    { location :: a
    , dependencies :: List CanonicalModule
    }


newtype Location =
  Location
    { relativePath :: FilePath
    , package :: Package
    }



-- BUILD-FRIENDLY SUMMARY


newtype BuildGraph =
  BuildGraph
    { blockedModules :: Map CanonicalModule BuildData
    , completedInterfaces :: Map CanonicalModule Module.Interface
    }


newtype BuildData =
  BuildData
    { blocking :: List CanonicalModule
    , location :: Location
    }
