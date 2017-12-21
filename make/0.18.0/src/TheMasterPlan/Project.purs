module TheMasterPlan.Project where

import Ellie.Prelude

import Control.Monad.Task (Task)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, get, put)
import Data.Foreign.Index ((!))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Elm.Package (Package(..))
import System.FileSystem ((</>), FILESYSTEM)
import System.FileSystem as FileSystem
import TheMasterPlan.CanonicalModule (CanonicalModule)
import TheMasterPlan.CanonicalModule as CanonicalModule
import TheMasterPlan.Location (Location)
import TheMasterPlan.Location as Location


newtype ProjectGraph a =
  ProjectGraph
    { data :: Map CanonicalModule (ProjectData a)
    , natives :: Map CanonicalModule Location
    }

derive instance eqProjectGraph :: Eq a => Eq (ProjectGraph a)
derive instance ordProjectGraph :: Ord a => Ord (ProjectGraph a)
derive instance newtypeProjectGraph :: Newtype (ProjectGraph a) _

instance foreignableProjectGraph :: Foreignable a => Foreignable (ProjectGraph a) where
  put (ProjectGraph { data: d, natives }) =
    Foreign.toForeign
      { data: put d
      , natives: put natives
      }
  
  get value =
    { data: _, natives: _ }
      <$> (value ! "data" >>= get)
      <*> (value ! "natives" >>= get)
      <#> ProjectGraph


union :: ∀ a. ProjectGraph a -> ProjectGraph a -> ProjectGraph a
union (ProjectGraph left) (ProjectGraph right) =
  ProjectGraph
    { data: (Map.union left.data right.data)
    , natives: (Map.union left.natives right.natives)
    }


load ::
  ∀  a e
  .  Foreignable a
  => Package
  -> Task (fileSystem :: FILESYSTEM | e) FileSystem.Error (ProjectGraph a)
load (Package { name, version }) =
  FileSystem.read
    ("/elm-stuff/packages/" </> show name </> show version </> "graph.data")


save ::
  ∀  a e
  .  Foreignable a
  => Package
  -> ProjectGraph a
  -> Task (fileSystem :: FILESYSTEM | e) FileSystem.Error Unit
save (Package { name, version }) graph =
  FileSystem.write
    ("/elm-stuff/packages/" </> show name </> show version </> "graph.data")
    graph


isSaved ::
  ∀ e x
  . Package
  -> Task (fileSystem :: FILESYSTEM | e) x Boolean
isSaved (Package { name, version }) =
  FileSystem.exists
    ("/elm-stuff/packages/" </> show name </> show version </> "graph.data")


newtype ProjectData a =
  ProjectData
    { location :: a
    , dependencies :: Array CanonicalModule
    }

derive instance eqProjectData :: Eq a => Eq (ProjectData a)
derive instance ordProjectData :: Ord a => Ord (ProjectData a)
derive instance newtypeProjectData :: Newtype (ProjectData a) _


instance foreignableProjectData :: Foreignable a => Foreignable (ProjectData a) where
  put (ProjectData { location, dependencies }) =
    Foreign.toForeign
      { location: put location
      , dependencies: put dependencies
      }

  get value =
    { location: _, dependencies: _ }
      <$> (value ! "location" >>= get)
      <*> (value ! "dependencies" >>= get)
      <#> ProjectData
