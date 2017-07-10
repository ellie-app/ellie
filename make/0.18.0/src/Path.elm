module Path exposing (..)

import Data.FilePath as FilePath exposing (FilePath, (</>), (<.>))
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Elm.Make.Location as Location exposing (Location)
import Elm.Compiler.Module as Module
import Elm.Package as Package exposing (Package)
import Elm.Package.Name as Name exposing (Name)
import Elm.Package.Version as Version exposing (Version)


toInterface : FilePath -> CanonicalModule -> FilePath
toInterface root { package, name } =
    root </> inPackage package (Module.hyphenate name <.> "elmi")


toObjectFile : FilePath -> CanonicalModule -> FilePath
toObjectFile root { package, name } =
    root </> inPackage package (Module.hyphenate name <.> "elmo")


toPackageCacheFile : FilePath -> Package -> FilePath
toPackageCacheFile root package =
    root </> inPackage package "graph.dat"


toSource : Location -> FilePath
toSource { relativePath, package } =
    relativePath


inPackage : Package -> FilePath -> FilePath
inPackage ( name, version ) relativePath =
    Name.toFilePath name </> Version.toString version </> relativePath


toVersionsList : Name -> FilePath
toVersionsList name =
    Name.toFilePath name </> "versions.json"
