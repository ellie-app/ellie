module System.FileSystem
  ( FILESYSTEM
  , read
  , exists
  , modified
  , module FilePath
  ) where

import Prelude
import Data.Function.Uncurried (Fn3, runFn3)
import Control.Monad.Eff (kind Effect)
import Control.Monad.Aff (Aff)
import System.FilePath (FilePath)
import System.FilePath as FilePath
import Data.Maybe (Maybe(..))
import Data.DateTime.Instant (Instant)
import Data.Argonaut (Json)

foreign import data FILESYSTEM :: Effect


read :: forall e. FilePath -> Aff (fileSystem :: FILESYSTEM | e) Json
read path =
  path
    # FilePath.normalize
    # _read


exists :: forall e. FilePath -> Aff (fileSystem :: FILESYSTEM | e) Boolean
exists path =
  path
    # FilePath.normalize
    # _doesFileExist



modified :: forall e. FilePath -> Aff (fileSystem :: FILESYSTEM | e) (Maybe Instant)
modified path =
  path
    # FilePath.normalize
    # runFn3 _getModificationTime Nothing Just


foreign import _read
  :: forall e
  .  FilePath
  -> Aff (fileSystem :: FILESYSTEM | e) Json


foreign import _doesFileExist
  :: forall e
  .  FilePath
  -> Aff (fileSystem :: FILESYSTEM | e) Boolean


foreign import _getModificationTime
    :: forall e.
       Fn3
        (Maybe Instant)
        (Instant -> Maybe Instant)
        FilePath
        (Aff (fileSystem :: FILESYSTEM | e) (Maybe Instant))
