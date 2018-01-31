module Ellie.Adapters.LocalRepo where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadThrow, catchError)
import Control.Monad.Except (throwError)
import Control.Monad.Except as Except
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class (liftIO) as IO
import Data.Array as Array
import Data.Either as Either
import Data.Entity (Entity)
import Data.Entity as Entity
import Data.FilePath (FilePath, (</>), (<.>))
import Data.Foldable (maximum)
import Data.Foreign.Generic (decodeJSON, encodeJSON) as Foreign
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (stripSuffix) as String
import Data.String.Class (toString) as String
import Data.UniqueId (UniqueId(..))
import Ellie.Types.ProjectId (ProjectId)
import Ellie.Types.Revision (Revision)
import Ellie.Types.Revision as Revision
import Ellie.Types.User (User)
import Ellie.Types.User as User
import System.FileSystem as FileSystem
import System.Jwt (Jwt, Secret)
import System.Jwt as Jwt
import System.UniqueId as UniqueId


type Env r =
  { jwtSecret ∷ Secret
  | r
  }


localData :: ∀ m. MonadIO m => m FilePath
localData = IO.liftIO do
  cwd <- FileSystem.cwd
  let path = cwd </> ".local_data"
  exists <- FileSystem.exists path
  if not exists
    then do
      FileSystem.createDirectory path
      FileSystem.createDirectory (path </> "users")
      FileSystem.createDirectory (path </> "revisions")
    else pure unit
  pure path


getRevision :: ∀ m. MonadIO m => MonadThrow Error m => Revision.Id -> m (Maybe (Entity Revision.Id Revision))
getRevision revisionId = IO.liftIO do
  basePath <- localData
  let path = basePath </> "revisions" </> show revisionId <.> "json"
  exists <- FileSystem.exists path
  if not exists
    then pure Nothing
    else do
      revisionString <- FileSystem.read path
      revisionString
        # Foreign.decodeJSON
        # Except.runExcept
        # Either.either (renderError revisionId) pure
        # map (Entity.entity revisionId >>> Just)
  where
    renderError revisionId multipleErrors =
      throwError $ error $ "Corrupted revision with ID " <> show revisionId


getLatestRevisionNumber :: ∀ m. MonadIO m => ProjectId -> m (Maybe Int)
getLatestRevisionNumber projectId = do
  basePath <-
    localData
  entries <-
    IO.liftIO $ FileSystem.readDirectory (basePath </> "revisions" </> show projectId)
  pure $ findLatest entries
  where
    processKey :: FilePath -> Maybe Int
    processKey key =
      key
        # String.stripSuffix (Pattern ".json")
        >>= String.stripSuffix (Pattern ".html")
        >>= (Foreign.decodeJSON >>> Except.runExcept >>> Either.hush)

    findLatest :: Array String -> Maybe Int
    findLatest keys =
      keys
        # Array.mapMaybe processKey
        # maximum


revisionExists :: ∀ m. MonadIO m => Revision.Id -> m Boolean
revisionExists revisionId = do
  basePath <- localData
  IO.liftIO $ FileSystem.exists $ basePath </> "revisions" </> show revisionId <.> "json"


saveRevision :: ∀ m. MonadIO m => Revision.Id -> Revision -> m Unit
saveRevision revisionId revision = IO.liftIO do
  basePath <- localData
  let path = basePath </> "revisions" </> show revisionId <.> "json"
  FileSystem.write path (Foreign.encodeJSON revision)


-- USERS


getUser ∷ ∀ m. MonadIO m ⇒ MonadThrow Error m ⇒ User.Id → m (Maybe (Entity User.Id User))
getUser userId@(User.Id uuid) = IO.liftIO do
  basePath <- localData
  let path = basePath </> "users" </> String.toString uuid <.> "json"
  exists ← FileSystem.exists path
  if not exists
    then pure Nothing
    else do
      fsValue ← FileSystem.read path
      fsValue
        # Foreign.decodeJSON
        # Except.runExcept
        # Either.either (decodeError uuid) pure
        # map (Entity.entity userId >>> Just)
  where
    decodeError uuid _ =
      throwError $ error $ "Corrupted user with ID: " <> String.toString uuid


createUser ∷ ∀ m. MonadIO m ⇒ m (Entity User.Id User)
createUser = IO.liftIO do
  basePath ← localData
  uuid ← UniqueId.uniqueId
  let path = basePath </> "users" </> String.toString uuid <.> "json"
  FileSystem.write path (Foreign.encodeJSON User.default)
  pure $ Entity.entity (User.Id uuid) User.default


saveUser ∷ ∀ m. MonadIO m ⇒ User.Id → User → m Unit
saveUser (User.Id uuid) user = IO.liftIO do
  basePath ← localData
  let path = basePath </> "users" </> String.toString uuid <.> "json"
  FileSystem.write path (Foreign.encodeJSON user)


verifyUser ∷ ∀ m r. MonadIO m ⇒ Jwt → Env r → m (Maybe User.Id)
verifyUser token env = IO.liftIO do
  maybeUserId ← (Just <$> Jwt.decode env.jwtSecret token) `catchError` \_ -> pure Nothing
  case maybeUserId of
    Just userId → getUser (User.Id (UniqueId userId)) <#> (map Entity.key)
    Nothing → pure Nothing


signUser ∷ ∀ m r. MonadIO m ⇒ User.Id → Env r → m Jwt
signUser (User.Id (UniqueId userIdString)) env =
  IO.liftIO $ Jwt.encode env.jwtSecret userIdString