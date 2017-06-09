module Data.Elm.Package.Paths exposing (..)

import Data.FilePath exposing (FilePath, (</>))
import Data.Elm.Package.Name as Name exposing (Name)
import Data.Elm.Package.Version as Version exposing (Version)


stuffDirectory : FilePath
stuffDirectory =
    "elm-stuff"


solvedDependencies : FilePath
solvedDependencies =
    stuffDirectory </> "exact-dependencies.json"


documentation : FilePath
documentation =
    stuffDirectory </> "documentation.json"


description : FilePath
description =
    "elm-package.json"


downloadMarker : FilePath
downloadMarker =
    ".downloaded"


packagesDirectory : FilePath
packagesDirectory =
    stuffDirectory </> "packages"


package : Name -> Version -> FilePath
package name version =
    packagesDirectory </> Name.toFilePath name </> Version.toString version
