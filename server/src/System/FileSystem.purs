module System.FileSystem where

import Prelude
import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class as Eff
import Control.Monad.IO (IO)


foreign import data FILESYSTEM :: Effect
foreign import _read :: ∀ e. FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) String
foreign import _write :: ∀ e. Unit -> FilePath -> String -> EffFnAff (fileSystem :: FILESYSTEM | e) Unit
foreign import _exists :: ∀ e. FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) Boolean
foreign import _createDirectory :: ∀ e. Unit -> FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) Unit
foreign import _unzip :: ∀ e. Unit -> FilePath -> FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) Unit
foreign import _remove :: ∀ e. Unit -> FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) Unit
foreign import _cwd :: ∀ e. Eff (fileSystem :: FILESYSTEM | e) FilePath
foreign import _readDirectory :: ∀ e. FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) (Array FilePath)
foreign import _createTemporaryDirectory ∷ ∀ e. EffFnAff (fileSystem ∷ FILESYSTEM | e) FilePath

type FilePath = String


read :: FilePath -> IO String
read path = Aff.liftAff $ Aff.fromEffFnAff $ _read path


write :: FilePath -> String -> IO Unit
write path contents = Aff.liftAff $ Aff.fromEffFnAff $ _write unit path contents


exists :: FilePath -> IO Boolean
exists path = Aff.liftAff $ Aff.fromEffFnAff $ _exists path


createDirectory :: FilePath -> IO Unit
createDirectory path = Aff.liftAff $ Aff.fromEffFnAff $ _createDirectory unit path


unzip :: FilePath -> FilePath -> IO Unit
unzip from to = Aff.liftAff $ Aff.fromEffFnAff $ _unzip unit from to


remove :: FilePath -> IO Unit
remove path = Aff.liftAff $ Aff.fromEffFnAff $ _remove unit path


cwd :: IO FilePath
cwd = Eff.liftEff _cwd


readDirectory :: FilePath -> IO (Array FilePath)
readDirectory path = Aff.liftAff $ Aff.fromEffFnAff $ _readDirectory path


createTemporaryDirectory ∷ IO FilePath
createTemporaryDirectory = Aff.liftAff $ Aff.fromEffFnAff _createTemporaryDirectory