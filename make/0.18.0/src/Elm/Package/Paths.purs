module Elm.Package.Paths where

import Prelude (show)
import System.FileSystem (FilePath, (</>))
import Elm.Package.Version (Version)
import Elm.Package.Name (Name)

stuffDirectory :: FilePath
stuffDirectory =
    "elm-stuff"

solvedDependencies :: FilePath
solvedDependencies =
    stuffDirectory </> "exact-dependencies.json"

documentation :: FilePath
documentation =
    stuffDirectory </> "documentation.json"

description :: FilePath
description =
    "elm-package.json"

packagesDirectory :: FilePath
packagesDirectory =
    stuffDirectory </> "packages"

package :: Name -> Version -> FilePath
package name version =
    packagesDirectory </> show name </> show version
