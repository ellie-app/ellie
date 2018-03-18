module Ellie.Adapters.DatabaseSearch
  ( Env
  , search
  ) where

import Prelude

import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class (liftIO) as IO
import Data.Either (Either)
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Time.Good (Posix)
import Data.Time.Good (Span) as Time
import Data.Traversable (maximum)
import Elm.Name (Name)
import Elm.Name as Name
import Elm.Package (Package(..))
import Elm.Package.Searchable (Searchable(..))
import Elm.Version (Version)
import Elm.Version as Version
import System.Postgres as Postgres


type Env r =
  { packageSite ∷ String
  , postgresClient ∷ Postgres.Client
  , lastSearchUpdate ∷ Ref (Maybe Posix)
  , searchRefresh ∷ Time.Span
  | r
  }


search ∷ ∀ m r. MonadIO m ⇒ MonadThrow Error m ⇒ String → Env r → m (Array Searchable)
search query env = IO.liftIO do
  pure []
  where
    toSearchable ∷ { description ∷ String, name ∷ Name, versions ∷ Array Version } → Searchable
    toSearchable { name, description, versions } =
      Searchable
        { package:
            Package
              { name: name
              , version: Maybe.fromMaybe Version.zero $ maximum versions
              }
        , description
        }

    decodeNameAndVersion ∷ Json → Either Json.Error { description ∷ String, name ∷ Name, versions ∷ Array Version }
    decodeNameAndVersion value = do
      { name: _, versions: _, description: _ }
        <$> Json.decodeAtField "name" value Name.fromBody
        <*> Json.decodeAtField "versions" value (Json.decodeArray Version.fromBody)
        <*> Json.decodeAtField "summary" value Json.decodeString

    decodeNameAndVersions ∷ Json → Either Json.Error (Array { description ∷ String, name ∷ Name, versions ∷ Array Version })
    decodeNameAndVersions array =
      Json.decodeArray decodeNameAndVersion array
