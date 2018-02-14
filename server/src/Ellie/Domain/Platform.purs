module Ellie.Domain.Platform
  ( class Platform
  , initialize
  , destroy
  , compile
  , format
  ) where


import Prelude
import Data.Either (Either)
import Ellie.Elm.Compiler.Error as Compiler
import Ellie.Types.Workspace (Workspace)

class Platform m where
  initialize ∷ m Workspace
  destroy ∷ Workspace → m Unit
  compile ∷ String → String → Workspace → m (Array Compiler.Error)
  format ∷ String → m (Either String String)