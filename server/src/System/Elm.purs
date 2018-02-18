module System.Elm
  ( init
  , compile
  , install
  , format
  ) where


import Prelude

import Control.Monad.Aff.Class as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.IO (IO)
import Control.Monad.IO as IO
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IO.Effect as Control.Monad.IO.Effect
import Data.Either (Either(..))
import Data.FilePath (FilePath)
import Data.Foreign (Foreign)
import Data.Foreign.Class (encode) as Foreign
import Ellie.Elm.Package (Package(..))


foreign import _init ∷ ∀ e. { helpers ∷ FfiHelpers, root ∷ FilePath } → EffFnAff (elm ∷ INFINITY | e) (Either String String)
foreign import _install ∷ ∀ e. { helpers ∷ FfiHelpers, root ∷ FilePath, name ∷ Foreign, version ∷ Foreign } → EffFnAff (elm ∷ INFINITY | e) (Either String String)
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


install ∷ FilePath → Package → IO (Either String String)
install root (Package p) =
  Aff.liftAff $
    Aff.fromEffFnAff $
      _install
        { name: Foreign.encode p.name
        , version: Foreign.encode p.version
        , root
        , helpers
        }


compile ∷
  { root ∷ FilePath
  , entry ∷ FilePath
  , output ∷ FilePath
  , debug ∷ Boolean
  }
  → IO (Either String String)
compile { root, entry, debug, output } =
  Aff.liftAff $ Aff.fromEffFnAff $ _compile { helpers, root, output, entry, debug }


format ∷ String → IO (Either String String)
format code =
  Aff.liftAff $ Aff.fromEffFnAff $ _format { helpers, code }