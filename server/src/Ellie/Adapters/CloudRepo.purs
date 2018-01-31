module Ellie.Adapters.CloudRepo
  ( Env
  , getRevision
  , getLatestRevisionNumber
  , revisionExists
  , saveRevision
  , getUser
  , createUser
  , saveUser
  , verifyUser
  , signUser
  ) where

import Prelude

import Control.Monad.Aff (catchError, try)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Control.Monad.Except as Except
import Control.Monad.IO (IO)
import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class (liftIO) as IO
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Entity (Entity)
import Data.Entity as Entity
import Data.FilePath (FilePath, (<.>), (</>))
import Data.FilePath as FilePath
import Data.Foldable (maximum)
import Data.Foreign (Foreign)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Foreign.Generic (decodeJSON, encodeJSON) as Foreign
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Class (toString) as String
import Data.UniqueId (UniqueId(..))
import Ellie.Types.ProjectId (ProjectId)
import Ellie.Types.Revision (Revision)
import Ellie.Types.Revision as Revision
import Ellie.Types.User (User)
import Ellie.Types.User as User
import System.Aws as Aws
import System.Jwt (Jwt, Secret)
import System.Jwt as Jwt
import System.Redis as Redis
import System.UniqueId as UniqueId


type Env r =
  { redisClient ∷ Redis.Client
  , awsClient ∷ Aws.Client
  , bucket ∷ String
  , jwtSecret ∷ Secret
  | r
  }


liftAws :: ∀ m a. MonadIO m => MonadThrow Error m => IO (Either Aws.Error a) -> m a
liftAws awsCommand =
  awsCommand
    # IO.liftIO
    >>= Either.either handleError pure
  where
    handleError e =
      throwError $ error e.message


-- REVISIONS


getRevision :: ∀ m r. MonadIO m => MonadThrow Error m => Revision.Id -> Env r -> m (Maybe (Entity Revision.Id Revision))
getRevision revisionId@(Revision.Id { projectId, revisionNumber }) env = IO.liftIO do
  jsonStringOrError ←
    Aws.getObject env.awsClient
      { key: "/revisions" </> String.toString projectId </> String.toString revisionNumber <.> "json"
      , bucket: env.bucket
      }
  case jsonStringOrError of
    Left e | e.code == "404" → pure Nothing
    Left e → throwError $ error e.message
    Right jsonString →
      jsonString
        # Foreign.decodeJSON
        # Except.runExcept
        # Either.either (decodeError revisionId) pure
        # map (Entity.entity revisionId >>> Just)
  where
    decodeError revisionId _ =
      throwError $ error $ "Corrupted revision at ID " <> show revisionId


getLatestRevisionNumber ::
  ∀  m r
  .  MonadIO m
  => MonadThrow Error m
  => ProjectId
  -> Env r
  -> m (Maybe Int)
getLatestRevisionNumber pid env = do
  let
    redisKey =
      "LatestRevisionNumber:" <> show pid
  let
    awsKey =
      "revisions" </> show pid
  maybeRedisValue <-
    Redis.get env.redisClient redisKey
      # map (_ >>= decodeRedisValue)
      # IO.liftIO
  case maybeRedisValue of
    Just value ->
      pure (Just value)
    Nothing -> do
      objects <-
        liftAws $
          Aws.listObjects env.awsClient { prefix: awsKey, bucket: env.bucket }
      let
        latest =
          findLatest objects
      case latest of
        Just value -> do
          IO.liftIO $
            Redis.set env.redisClient redisKey (Foreign.encode value)
          pure $ Just value
        Nothing ->
          pure $ Nothing
  where
    processKey :: FilePath -> Maybe Int
    processKey key =
      key
        # FilePath.basename
        # String.stripSuffix (Pattern ".json")
        >>= String.stripSuffix (Pattern ".html")
        >>= (Foreign.decodeJSON >>> Except.runExcept >>> Either.hush)

    findLatest :: Array String -> Maybe Int
    findLatest keys =
      keys
        # Array.mapMaybe processKey
        # maximum

    decodeRedisValue :: Foreign -> Maybe Int
    decodeRedisValue value =
      value
        # Foreign.decode
        # Except.runExcept
        # Either.hush


revisionExists :: ∀ m r. MonadIO m => MonadThrow Error m => Revision.Id -> Env r -> m Boolean
revisionExists revisionId@(Revision.Id { projectId, revisionNumber }) env = do
  redisKeyExists <-
    ("LatestRevisionNumber:" <> show projectId)
      # Redis.exists env.redisClient
      # IO.liftIO
  if redisKeyExists
    then pure true
    else do
      unitOrError <-
        IO.liftIO $
          Aws.headObject env.awsClient
            { key: "/revisions" </> show projectId </> show revisionNumber <.> "json"
            , bucket: env.bucket
            }
      case unitOrError of
        Left e | e.code == "404" -> pure $ false 
        Left e -> Except.throwError (error e.message)
        Right _ -> pure  true


saveRevision :: ∀ m r. MonadIO m => MonadThrow Error m => Revision.Id -> Revision -> Env r -> m Unit
saveRevision revisionId@(Revision.Id { projectId, revisionNumber }) revision env = do
  unitOrError <-
    IO.liftIO $
      Aws.putObject env.awsClient
        { key: "/revisions" </> show revisionId <.> "json"
        , bucket: env.bucket
        , contents: Foreign.encodeJSON revision
        , mimeType: "application/json"
        }

  case unitOrError of
    Right _ -> pure unit
    Left e -> throwError $ error e.message

  Redis.set env.redisClient ("LatestRevisionNumber:" <> show projectId) (Foreign.encode revisionNumber)
    # try
    # void
    # IO.liftIO


-- USERS


getUser ∷ ∀ m r. MonadIO m ⇒ MonadThrow Error m ⇒ User.Id → Env r → m (Maybe (Entity User.Id User))
getUser userId@(User.Id uuid) env = IO.liftIO do
  let redisKey = "Users:" <> String.toString uuid
  maybeRedisValue ←
    Redis.get env.redisClient redisKey
  case maybeRedisValue of
    Just redisValue →
      redisValue
        # Foreign.decode
        # Except.runExcept
        # Either.either (decodeError uuid) pure
        # map (Entity.entity userId >>> Just)
    Nothing →
      pure Nothing
  where
    decodeError uuid _ =
      throwError $ error $ "Corrupted user with ID: " <> String.toString uuid


createUser ∷ ∀ m r. MonadIO m ⇒ Env r → m (Entity User.Id User)
createUser env = IO.liftIO do
  uuid ← UniqueId.uniqueId
  let redisKey = "Users:" <> String.toString uuid
  Redis.set env.redisClient redisKey (Foreign.encode User.default)
  pure $ Entity.entity (User.Id uuid) User.default


saveUser ∷ ∀ m r. MonadIO m ⇒ User.Id → User → Env r → m Unit
saveUser (User.Id uuid) user env = IO.liftIO do
  let redisKey = "Users:" <> String.toString uuid
  Redis.set env.redisClient redisKey (Foreign.encode user)


verifyUser ∷ ∀ m r. MonadIO m ⇒ Jwt → Env r → m (Maybe User.Id)
verifyUser token env = IO.liftIO do
  maybeUserId ← (Just <$> Jwt.decode env.jwtSecret token) `catchError` \_ -> pure Nothing
  case maybeUserId of
    Just userId → getUser (User.Id (UniqueId userId)) env <#> (map Entity.key)
    Nothing → pure Nothing


signUser ∷ ∀ m r. MonadIO m ⇒ User.Id → Env r → m Jwt
signUser (User.Id (UniqueId userIdString)) env =
  IO.liftIO $ Jwt.encode env.jwtSecret userIdString
