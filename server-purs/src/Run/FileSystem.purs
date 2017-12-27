module Run.FileSystem
  ( FILESYSTEM
  , FileSystem
  , FilePath
  , read
  , write
  , exists
  , createDirectory
  , interpretAff
  ) where


import Prelude
import Run (FProxy, Run, SProxy(..))
import Run as Run
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.FileSystem as FileSystemAff


type FilePath = String


type FILESYSTEM = FProxy FileSystem


_fileSystem = SProxy :: SProxy "fileSystem"


data FileSystem a
  = Read FilePath (String -> a)
  | Write FilePath String a
  | Exists FilePath (Boolean -> a)
  | CreateDirectory FilePath a
derive instance functorFileSystem :: Functor FileSystem


read :: ∀ r. FilePath -> Run (fileSystem :: FILESYSTEM | r) String
read path = Run.lift _fileSystem (Read path id)


write :: ∀ r. FilePath -> String -> Run (fileSystem :: FILESYSTEM | r) Unit
write path contents = Run.lift _fileSystem (Write path contents unit)


exists :: ∀ r. FilePath -> Run (fileSystem :: FILESYSTEM | r) Boolean
exists path = Run.lift _fileSystem (Exists path id)


createDirectory :: ∀ r. FilePath -> Run (fileSystem :: FILESYSTEM | r) Unit
createDirectory path = Run.lift _fileSystem (CreateDirectory path unit)


interpretAff :: ∀ e. FileSystem ~> Aff (fileSystem :: FileSystemAff.FILESYSTEM | e)
interpretAff command =
  case command of
    Read path next -> next <$> FileSystemAff.read path
    Write path contents next -> next <$ FileSystemAff.write path contents
    Exists path next -> next <$> FileSystemAff.exists path
    CreateDirectory path next -> next <$ FileSystemAff.createDirectory path