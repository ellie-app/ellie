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
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Class (toString) as String
import Data.Traversable (traverse)
import Elm.Compiler.Error as Compiler
import Elm.Package (Package(..))
import Elm.Package.Description (Description(..))
import System.FileSystem as FileSystem


foreign import _init ∷ ∀ e. { helpers ∷ FfiHelpers, root ∷ FilePath } → EffFnAff (elm ∷ INFINITY | e) (Either String String)
foreign import _compile ∷ ∀ e. { helpers ∷ FfiHelpers, root ∷ FilePath, entry ∷ FilePath, output ∷ FilePath, debug ∷ Boolean } → EffFnAff (elm ∷ INFINITY | e) (Either String String)
foreign import _format ∷ ∀ e. { helpers ∷ FfiHelpers, code ∷ String } → EffFnAff (elm ∷ INFINITY | e) (Either String String)


type FfiHelpers =
  { left ∷ ∀ x a. x → Either x a
  , right ∷ ∀ x a. a → Either x a
  }


helpers ∷ FfiHelpers
helpers =
  { left: Left
  , right: Right
  }


init ∷ FilePath → IO (Either String String)
init root =
  Aff.liftAff $ Aff.fromEffFnAff $ _init { helpers, root }


install ∷ FilePath → Set Package → IO Unit
install root packages = do
  (Description d) ← readDescription root
  if packages /= d.dependencies
    then do
      let updated = d { dependencies = packages }
      writeDescription root (Description updated)
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
    # readDescription
    <#> (unwrap >>> _.dependencies)


deleteExactDeps ∷ FilePath → IO Unit
deleteExactDeps root =
  FileSystem.remove (root </> "elm-stuff" </> "exact-dependencies.json")


readDescription ∷ FilePath → IO Description
readDescription root = do
  rawDescription ← FileSystem.read (root </> "elm-package.json")
  rawDescription
    # Foreign.decodeJSON
    # Except.runExcept
    # Either.either ((\e → error $ "Corrupted description: " <> show e) >>> Except.throwError) pure


writeDescription ∷ FilePath → Description → IO Unit
writeDescription root description =
  FileSystem.write (root </> "elm-package.json") $ Foreign.encodeJSON description
