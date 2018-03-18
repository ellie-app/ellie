module Ellie.Adapters.DatabaseSearch
  ( Env
  , search
  ) where

import Prelude

import Control.Monad.Eff.Class as Eff
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Error.Class as Error
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class (liftIO) as IO
import Data.Either (Either(..))
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Time.Good (Posix)
import Data.Time.Good (Span, diff) as Time
import Elm.Package.Searchable (Searchable)
import Elm.Package.Searchable as Searchable
import System.Http as Http
import System.Postgres as Postgres
import System.Time (now) as Time


type Env r =
  { packageSite ∷ String
  , postgresClient ∷ Postgres.Client
  , lastSearchUpdate ∷ Ref (Maybe Posix)
  , searchRefresh ∷ Time.Span
  | r
  }


search ∷ ∀ m r. MonadIO m ⇒ MonadThrow Error m ⇒ String → Env r → m (Array Searchable)
search query env = IO.liftIO do
  now ← Time.now
  maybeLastSearchUpdate ← Eff.liftEff $ Ref.readRef env.lastSearchUpdate
  case maybeLastSearchUpdate of
    Just lastUpdate | Time.diff now lastUpdate < env.searchRefresh →
      Postgres.exec env.postgresClient (Json.decodeArray Searchable.fromPostgres)
        $ Postgres.invoke "ellie.searchable_packages_search"
        $ [ { key: "query", value: Json.encodeString query }
          , { key: "threshold", value: Json.encodeNumber 0.3 }
          ]

    _ → do
      rawSearchables ← Http.get (env.packageSite <> "/search.json")
      let searchables = Json.decodeArray Searchable.fromBody rawSearchables
      case searchables of
        Left error →
          Error.throwError
            $ Exception.error
            $ Json.errorToString error

        Right values → do
          Postgres.exec env.postgresClient (const (Right unit))
            $ Postgres.invoke "ellie.searchable_packages_populate"
            $ [ { key: "packages", value: Json.encodeArray Searchable.toPostgres values } ]
          Eff.liftEff $ Ref.writeRef env.lastSearchUpdate (Just now)
          search query env
