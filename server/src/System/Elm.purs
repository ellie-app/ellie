module System.Elm
  ( init
  , compile
  , format
  , install
  , dependencies
  ) where


import Prelude

import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except as Except
import Control.Monad.IO (IO)
import Control.Monad.IO.Effect (INFINITY)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.FilePath (FilePath, (</>))
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String (charAt, split) as String
import Data.Traversable (traverse)
import Elm.Compiler.Error as Compiler
import Elm.Name (Name)
import Elm.Name as Name
import Elm.Package (Package)
import Elm.Project (Project(..))
import Elm.Project as Project
import System.FileSystem as FileSystem


foreign import _installByName ∷ ∀ e. { helpers ∷ FfiHelpers, root ∷ FilePath, name ∷ String } → EffFnAff (elm ∷ INFINITY | e) Unit
foreign import _compile ∷ ∀ e. { helpers ∷ FfiHelpers, root ∷ FilePath, entry ∷ FilePath, output ∷ FilePath, debug ∷ Boolean } → EffFnAff (elm ∷ INFINITY | e) (Either String String)
foreign import _format ∷ ∀ e. { helpers ∷ FfiHelpers, code ∷ String } → EffFnAff (elm ∷ INFINITY | e) (Either String String)
foreign import _reinstall ∷ ∀ e. { helpers ∷ FfiHelpers, root ∷ FilePath } → EffFnAff (elm ∷ INFINITY | e) Unit


type FfiHelpers =
  { left ∷ ∀ x a. x → Either x a
  , right ∷ ∀ x a. a → Either x a
  , unit ∷ Unit
  }


helpers ∷ FfiHelpers
helpers =
  { left: Left
  , right: Right
  , unit: unit
  }


installByName ∷ FilePath → Name → IO Unit
installByName root name =
  Aff.liftAff $ Aff.fromEffFnAff $ _installByName { helpers, root, name: Name.toString name }


init ∷ FilePath → IO Unit
init root = do
  FileSystem.write (root </> "elm.json") $ Project.toFile Project.default
  FileSystem.createDirectory (root </> "src")
  installByName root Name.core
  installByName root Name.browser
  installByName root Name.html


install ∷ FilePath → Set Package → IO Unit
install root packages = do
  (Project p) ← readProject root
  if packages /= p.deps
    then do
      let updated = p { deps = packages, transDeps = Set.empty }
      writeProject root (Project updated)
      FileSystem.remove (root </> "elm-stuff" </> "exact-dependencies.json")
    else
      pure unit


compile ∷
  { root ∷ FilePath
  , entry ∷ FilePath
  , output ∷ FilePath
  , debug ∷ Boolean
  }
  → IO (Array Compiler.Error)
compile { root, entry, debug, output } = do
  stderrOrStdout ←
    Aff.liftAff $ Aff.fromEffFnAff $ _compile { helpers, root, output, entry, debug }
  case stderrOrStdout of
    Left message →
      pure
        [ Compiler.Error
            { tag: "COMPILER ERROR"
            , message: message
            , level: "error"
            , region:
                Compiler.Region
                  { start: Compiler.Location { line: 1, column: 1 }
                  , end: Compiler.Location { line: 1, column: 7 }
                  }
            }
        ]

    Right text →
      text
        # String.split (Pattern "\n")
        # traverse
          (\line →
            if String.charAt 0 line == Just '[' && String.charAt 1 line == Just '{' then
              line
                # Json.parse
                >>= Json.decodeArray Compiler.decode
                # Either.either (Json.errorToString >>> error >>> Except.throwError) pure
            else
              pure []     
          )
        <#> Array.concat


format ∷ String → IO (Either String String)
format code =
  Aff.liftAff $ Aff.fromEffFnAff $ _format { helpers, code }


dependencies ∷ FilePath → IO (Set Package)
dependencies root =
  root
    # readProject
    <#> (unwrap >>> _.deps)


readProject ∷ FilePath → IO Project
readProject root = do
  rawProject ← FileSystem.read (root </> "elm.json")
  rawProject
    # Project.fromFile
    # Either.either ((\e → error $ "Corrupted project: " <> e) >>> Except.throwError) pure


writeProject ∷ FilePath → Project → IO Unit
writeProject root project =
  FileSystem.write (root </> "elm.json") $ Project.toFile project
