module Ellie.Types.Workspace where

import Data.FilePath (FilePath)
import Data.Newtype (class Newtype)
import Ellie.Elm.Package (Package)

newtype Workspace =
  Workspace
    { location ∷ FilePath
    , packages ∷ Array Package
    }

derive instance newtypeWorkspace ∷ Newtype Workspace _
