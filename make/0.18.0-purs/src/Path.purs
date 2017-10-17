module Path
  ( toInterface
  , toObjectFile
  , toPackageCacheFile
  , toSource
  )
  where

import Prelude (show)
import System.FilePath (FilePath, (</>), (<.>))
import Elm.Compiler.Module as Module
import Elm.Package as Package
import TheMasterPlan as TMP


toInterface :: FilePath -> TMP.CanonicalModule -> FilePath
toInterface root (TMP.CanonicalModule { package, name }) =
    root </> inPackage package (Module.hyphenate name <.> "elmi")


toObjectFile :: FilePath -> TMP.CanonicalModule -> FilePath
toObjectFile root (TMP.CanonicalModule { package, name }) =
    root </> inPackage package (Module.hyphenate name <.> "elmo")


toPackageCacheFile :: FilePath -> TMP.Package -> FilePath
toPackageCacheFile root package =
    root </> inPackage package "graph.dat"


toSource :: TMP.Location -> FilePath
toSource (TMP.Location { relativePath, package }) =
    relativePath


inPackage :: TMP.Package -> FilePath -> FilePath
inPackage (TMP.Package { package, version }) relativePath =
    Package.toFilePath package </> show version </> relativePath
