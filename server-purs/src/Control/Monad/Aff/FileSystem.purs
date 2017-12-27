module Control.Monad.Aff.FileSystem where

import Prelude
import Control.Monad.Eff (kind Effect)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat as Aff
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

foreign import data FILESYSTEM :: Effect
foreign import _read :: ∀ e. FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) String
foreign import _write :: ∀ e. Fn3 Unit FilePath String (EffFnAff (fileSystem :: FILESYSTEM | e) Unit)
foreign import _exists :: ∀ e. FilePath -> EffFnAff (fileSystem :: FILESYSTEM | e) Boolean
foreign import _createDirectory :: ∀ e. Fn2 Unit FilePath (EffFnAff (fileSystem :: FILESYSTEM | e) Unit)


type FilePath = String


read :: ∀ e. FilePath -> Aff (fileSystem :: FILESYSTEM | e) String
read path = Aff.fromEffFnAff $ _read path


write :: ∀ e. FilePath -> String -> Aff (fileSystem :: FILESYSTEM | e) Unit
write path contents = Aff.fromEffFnAff $ runFn3 _write unit path contents


exists :: ∀ e. FilePath -> Aff (fileSystem :: FILESYSTEM | e) Boolean
exists path = Aff.fromEffFnAff $ _exists path


createDirectory :: ∀ e. FilePath -> Aff (fileSystem :: FILESYSTEM | e) Unit
createDirectory path = Aff.fromEffFnAff $ runFn2 _createDirectory unit path