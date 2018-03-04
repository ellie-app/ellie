module System.Elm
  ( init
  , compile
  , format
  , install
  , dependencies
  ) where


import Prelude

import Control.Monad.Aff.Class as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except as Except
import Control.Monad.IO (IO)
import Control.Monad.IO as IO
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IO.Effect as Control.Monad.IO.Effect
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.FilePath (FilePath, (</>))
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign.Class (encode, decode) as Foreign
import Data.Foreign.Generic (encodeJSON, decodeJSON) as Foreign
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Class (toString) as String
import Data.Traversable (traverse)
import Elm.Compiler.Error as Compiler
import Elm.Package (Package(..))
import Elm.Package.Description (Description(..))
import Elm.Package.Name (Name)
import Elm.Package.Name as Name
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
  Aff.liftAff $ Aff.fromEffFnAff $ _installByName { helpers, root, name: String.toString name }


init ∷ FilePath → IO Unit
init root = do
  FileSystem.write (root </> "elm.json") $ Foreign.encodeJSON Project.default
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
            , overview: ""
            , details: message
            , level: "error"
            , subregion: Nothing
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
                # Foreign.decodeJSON
                # Except.runExcept
                # Either.either (\_ → throwError $ error "Unparseable compiler result") pure
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
    # Foreign.decodeJSON
    # Except.runExcept
    # Either.either ((\e → error $ "Corrupted project: " <> show e) >>> Except.throwError) pure


writeProject ∷ FilePath → Project → IO Unit
writeProject root project =
  FileSystem.write (root </> "elm.json") $ Foreign.encodeJSON project
