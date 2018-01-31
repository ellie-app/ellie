module Ellie.Domain.Platform
  ( class Platform
  , initialize
  , destroy
  , compile
  ) where


import Prelude
import Ellie.Elm.Compiler.Error as Compiler
import Ellie.Types.Workspace (Workspace)

class Platform m where
  initialize ∷ m Workspace
  destroy ∷ Workspace → m Unit
  compile ∷ String → String → Workspace → m (Array Compiler.Error)