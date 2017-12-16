module BuildManager where

import Prelude

import Control.Callback (CALLBACK)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Task as Task
import DOM.HTML.Event.EventTypes (message)
import Data.Blob (BLOB)
import Data.Maybe (Maybe)
import Elm.Compiler (COMPILER)
import Elm.Compiler.Error as Error
import Elm.Compiler.Module.Name.Raw (Raw)
import Elm.Compiler.Version as Compiler
import Elm.Package.Name (Name)
import Elm.Package.Paths as Path
import Ellie.SourceLoader as SourceLoader
import System.FileSystem (FILESYSTEM, FilePath, (</>))
import System.Http (HTTP)
import TheMasterPlan.CanonicalModule (CanonicalModule)


outputFilePath :: FilePath
outputFilePath =
  "/build.js"


artifactDirectory :: FilePath
artifactDirectory =
    Path.stuffDirectory </> "build-artifacts" </> (show Compiler.version)


type TopLevelEffect =
  ∀ e
  . Eff
    ( avar :: Task.AVAR
    , http :: HTTP
    , fileSystem :: FILESYSTEM
    , now :: NOW
    , compiler :: COMPILER
    , callback :: CALLBACK
    , blob :: BLOB
    | e
    )
    Unit


type Task a =
  ∀ e
  . Task.Task
    ( avar :: Task.AVAR
    , http :: HTTP
    , fileSystem :: FILESYSTEM
    , now :: NOW
    , compiler :: COMPILER
    , callback :: CALLBACK
    | e
    )
    Error
    a


data Error
    = CompilerInstallationError SourceLoader.Error
    | CompilerErrors FilePath String (Array Error.Error)
    | CorruptedArtifact FilePath
    | Cycle (Array CanonicalModule)
    | PackageProblem String
    | MissingPackage Name
    | ModuleNotFound Raw (Maybe Raw)
    | ModuleDuplicates
        { name :: Raw
        , parent :: Maybe Raw
        , local :: Array FilePath
        , foreign :: Array Name
        }
    | ModuleName
        { path :: FilePath
        , expectedName :: Raw
        , actualName :: Raw
        }
    | UnpublishablePorts FilePath Raw
    | UnpublishableEffects FilePath Raw
    | ImpossibleError String
    | CompilerCrash
        { name :: Raw
        , source :: String
        , message :: String
        }

instance showError :: Show Error where
    show (CompilerInstallationError error) = "CompilerInstallationError: " <> show error
    show (CompilerErrors _ _ _) = "CompilerErrors"
    show (CorruptedArtifact path) = "CorruptArtifact: " <> path
    show (Cycle _) = "Cycle"
    show (PackageProblem issue) = "PackageProblem: " <> issue
    show (MissingPackage _) = "MissingPackage"
    show (ModuleNotFound _ _) = "ModuleNotFound"
    show (ModuleDuplicates _) = "ModuleDuplicates"
    show (ModuleName _) = "ModuleName"
    show (UnpublishablePorts _ _) = "UnpublishablePorts"
    show (UnpublishableEffects _ _) = "UnpublishableEffects"
    show (ImpossibleError message) = "ImpossibleError: " <> message
    show (CompilerCrash _) = "CompilerCrash"