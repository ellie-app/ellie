module Pipeline.Install.Solution where

import Data.Map (Map)
import Elm.Package.Name (Name)
import Elm.Package.Version (Version)

type Solution =
  Map Name Version
