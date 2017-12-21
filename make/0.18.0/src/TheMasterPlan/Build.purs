module TheMasterPlan.Build where

import Ellie.Prelude
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Elm.Compiler.Module.Interface (Interface)
import TheMasterPlan.CanonicalModule (CanonicalModule)
import TheMasterPlan.Location (Location)


newtype BuildGraph =
  BuildGraph
    { blockedModules :: Map CanonicalModule BuildData
    , completedInterfaces :: Map CanonicalModule Interface
    }

derive instance newtypeBuildGraph :: Newtype BuildGraph _


newtype BuildData =
  BuildData
    { blocking :: Array CanonicalModule
    , location :: Location
    }

derive instance eqBuildData :: Eq BuildData
derive instance ordBuildData :: Ord BuildData
derive instance newtypeBuildData :: Newtype BuildData _
