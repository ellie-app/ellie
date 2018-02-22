module Ellie.BuildManager where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Error.Class (class MonadError, try)
import Control.Monad.Except as Except
import Control.Monad.IO (IO)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Entity as Entity
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (genericDecode, genericEncode, defaultOptions, decodeJSON, encodeJSON) as Foreign
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Url as Url
import Data.Url.Query as Query
import Ellie.Domain.Platform (class Platform)
import Ellie.Domain.Platform as Platform
import Ellie.Domain.UserRepo (class UserRepo)
import Ellie.Domain.UserRepo as UserRepo
import Ellie.Types.User as User
import Elm.Compiler.Error as Compiler
import Elm.Package (Package)
import Node.HTTP (Request)
import Node.HTTP as HTTP
import Server.Socket (Connection)
import System.Jwt (Jwt(..))


data Inbound
  = CompileRequested String String (Array Package)

derive instance genericInbound ∷ Generic Inbound _
instance decodeMsg ∷ Decode Inbound where decode = Foreign.genericDecode Foreign.defaultOptions


data Outbound
  = WorkspaceAttached (Array Package)
  | CompileSucceeded (Array Compiler.Error)
  | CompileFailed String
  | SaveSucceeded
  | ServerError String

derive instance genericOutbound ∷ Generic Outbound _
instance encodeOutbound ∷ Encode Outbound where encode = Foreign.genericEncode Foreign.defaultOptions


onAuthenticate ∷ ∀ m. UserRepo m ⇒ Monad m ⇒ Request → m (Maybe User.Id)
onAuthenticate request =
  let url = Url.parse $ HTTP.requestURL request
  in case Query.get "token" (Url.query url) of
    Just token → do
      maybeUserId ← UserRepo.verify (Jwt token)
      maybeEntity ← Maybe.maybe (pure Nothing) UserRepo.retrieve maybeUserId
      case maybeEntity of
        Just entity → pure $ Just $ Entity.key entity
        Nothing → pure Nothing
    Nothing →
      pure Nothing


onConnect ∷ ∀ m. Monad m ⇒ Platform m ⇒ User.Id → m Outbound
onConnect userId = do
  packages ← Platform.initialize userId
  pure $ WorkspaceAttached $ Array.fromFoldable packages


onDisconnect ∷ ∀ m. Monad m ⇒ Platform m ⇒ User.Id → m Unit
onDisconnect userId =
  Platform.destroy userId


onMessage ∷ ∀ m. Monad m ⇒ MonadError Error m ⇒ Platform m ⇒ User.Id → Inbound → m Outbound
onMessage userId inbound =
  case inbound of
    CompileRequested elm html packages → do
      failureOrErrors ← try $ Platform.compile elm html packages userId
      case failureOrErrors of
        Left failure → 
          pure $ CompileFailed $ Exception.message failure
        Right errors →
          pure $ CompileSucceeded errors


onError ∷ User.Id → Error → Outbound
onError userId error =
  ServerError $ Exception.message error


connection ∷ ∀ m. Monad m ⇒ Platform m ⇒ UserRepo m ⇒ MonadError Error m ⇒ (m ~> IO) → Connection User.Id
connection runner =
  { onAuthenticate: \request → runner $ onAuthenticate request
  , onDisconnect: \userId → runner $ onDisconnect userId
  , onConnect: \userId send → do
      outbound ← runner $ onConnect userId
      send $ Foreign.encodeJSON outbound
  , onMessage: \userId message send → do
      outbound ←
        message
          # Foreign.decodeJSON
          # Except.runExcept
          # lmap (\_ → error "Unparseable message")
          # Either.either (onError userId >>> pure) (onMessage userId)
          # runner
      send $ Foreign.encodeJSON outbound
  , onError: \userId error send →
      let outbound = onError userId error
      in send $ Foreign.encodeJSON outbound
  }
