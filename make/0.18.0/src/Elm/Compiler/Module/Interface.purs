module Elm.Compiler.Module.Interface where

import Ellie.Prelude

import Control.Monad.Task (Task)
import Data.Foreign.Class (class Foreignable, put, get)
import Data.Newtype (class Newtype, unwrap, wrap)
import System.FileSystem (FILESYSTEM, FilePath)
import System.FileSystem as FileSystem


newtype Interface =
  Interface String

derive instance newtypeInterface :: Newtype Interface _


instance foreignableInterface :: Foreignable Interface where
  put = unwrap >>> put
  get = get >>> map wrap


read :: ∀ e. FilePath -> Task (fileSystem :: FILESYSTEM | e) FileSystem.Error Interface
read path =
  FileSystem.read path