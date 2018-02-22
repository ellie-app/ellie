module Ellie.Domain.Platform
  ( class Platform
  , initialize
  , destroy
  , compile
  , format
  , result
  ) where


import Prelude

import Data.Either (Either)
import Data.Set (Set)
import Data.FilePath (FilePath)
import Ellie.Types.User as User
import Elm.Package (Package)
import Elm.Compiler.Error (Error)

class Platform m where
  initialize ∷ User.Id → m (Set Package)
  destroy ∷ User.Id → m Unit
  compile ∷ String → String → Array Package → User.Id → m (Array Error)
  format ∷ String → m (Either String String)
  result ∷ User.Id → m { javascript ∷ FilePath, html ∷ FilePath }
