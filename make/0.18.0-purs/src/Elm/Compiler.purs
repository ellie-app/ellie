{-# OPTIONS_GHC -Wall #-}
module Elm.Compiler
    ( version
    )
    where

import Elm.Package as Package

version :: Package.Version
version =
  Package.Version { major: 0, minor: 18, patch: 0 }
