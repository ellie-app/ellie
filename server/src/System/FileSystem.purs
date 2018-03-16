module System.FileSystem where

import Prelude
import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class as Eff
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Error.Class as Error
import Control.Monad.IO (IO)
import Data.Either (Either(..))
import Data.Either as Either


foreign import data FILESYSTEM ∷ Effect
foreign import _read ∷ ∀ e. FilePath → EffFnAff (fileSystem ∷ FILESYSTEM | e) String
foreign import _write ∷ ∀ e. Unit → FilePath → String → EffFnAff (fileSystem ∷ FILESYSTEM | e) Unit
foreign import _exists ∷ ∀ e. FilePath → EffFnAff (fileSystem ∷ FILESYSTEM | e) Boolean
foreign import _createDirectory ∷ ∀ e. Unit → FilePath → EffFnAff (fileSystem ∷ FILESYSTEM | e) Unit
foreign import _unzip ∷ ∀ e. Unit → FilePath → FilePath → EffFnAff (fileSystem ∷ FILESYSTEM | e) Unit
foreign import _remove ∷ ∀ e. Unit → FilePath → EffFnAff (fileSystem ∷ FILESYSTEM | e) Unit
foreign import _cwd ∷ ∀ e. Eff (fileSystem ∷ FILESYSTEM | e) FilePath
foreign import _readDirectory ∷ ∀ e. FilePath → EffFnAff (fileSystem ∷ FILESYSTEM | e) (Array FilePath)
foreign import _createTemporaryDirectory ∷ ∀ e. EffFnAff (fileSystem ∷ FILESYSTEM | e) FilePath

type FilePath = String


class IsFile a where
  toFile ∷ a → String
  fromFile ∷ String → Either String a

instance isFileString ∷ IsFile String where
  toFile = id
  fromFile = Right


read ∷ ∀ a. IsFile a ⇒ FilePath → IO a
read path =
  _read path
    # Aff.fromEffFnAff
    # Aff.liftAff
    >>= (fromFile >>> Either.either (Exception.error >>> Error.throwError) pure)


write ∷ ∀ a. IsFile a ⇒ FilePath → a → IO Unit
write path contents =
  Aff.liftAff
    $ Aff.fromEffFnAff
    $ _write unit path
    $ toFile contents


exists ∷ FilePath → IO Boolean
exists path = Aff.liftAff $ Aff.fromEffFnAff $ _exists path


createDirectory ∷ FilePath → IO Unit
createDirectory path = Aff.liftAff $ Aff.fromEffFnAff $ _createDirectory unit path


unzip ∷ FilePath → FilePath → IO Unit
unzip from to = Aff.liftAff $ Aff.fromEffFnAff $ _unzip unit from to


remove ∷ FilePath → IO Unit
remove path = Aff.liftAff $ Aff.fromEffFnAff $ _remove unit path


cwd ∷ IO FilePath
cwd = Eff.liftEff _cwd


readDirectory ∷ FilePath → IO (Array FilePath)
readDirectory path = Aff.liftAff $ Aff.fromEffFnAff $ _readDirectory path


createTemporaryDirectory ∷ IO FilePath
createTemporaryDirectory = Aff.liftAff $ Aff.fromEffFnAff _createTemporaryDirectory
