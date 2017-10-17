module Elm.Package.Paths where

import Prelude (show)
import System.FilePath (FilePath, (</>))
import Elm.Package as Package


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


package :: Package.Name -> Package.Version -> FilePath
package name version =
    packagesDirectory </> Package.toFilePath name </> show version
