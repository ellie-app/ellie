module Elm.Make.Error exposing (Error(..))

import Data.FilePath as FilePath exposing (FilePath)
import Elm.Package.Name as Name exposing (Name)
import Elm.Compiler.Module as Module
import Elm.Compiler.Error as CompilerError
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)


type Error
    = CompilerErrors FilePath String (List CompilerError.Error)
    | CorruptedArtifact FilePath
    | Cycle (List CanonicalModule)
    | PackageProblem String
    | MissingPackage Name
    | ModuleNotFound Module.Raw (Maybe Module.Raw)
    | ModuleDuplicates
        { name : Module.Raw
        , parent : Maybe Module.Raw
        , local : List FilePath
        , foreign : List Name
        }
    | ModuleName
        { path : FilePath
        , expectedName : Module.Raw
        , actualName : Module.Raw
        }
    | UnpublishablePorts FilePath Module.Raw
    | UnpublishableEffects FilePath Module.Raw
    | TotallyWeirdError
