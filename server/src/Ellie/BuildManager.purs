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
import Data.Foreign (Foreign)
import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class (encode) as Foreign
import Data.Foreign.Generic (genericDecode, genericEncode, defaultOptions, decodeJSON, encodeJSON) as Foreign
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show as Generic
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (Pattern(..))
import Data.String (contains) as String
import Data.Url as Url
import Data.Url.Query as Query
import Ellie.Domain.Platform (class Platform)
import Ellie.Domain.Platform as Platform
import Ellie.Domain.UserRepo (class UserRepo)
import Ellie.Domain.UserRepo as UserRepo
import Ellie.Types.User as User
import Elm.Package (Package)
import Node.HTTP (Request)
import Node.HTTP as HTTP
import Server.Socket (Connection)
import System.Jwt (Jwt(..))


data Inbound
  = CompileRequested String String (Array Package)

derive instance genericInbound ∷ Generic Inbound _
instance decodeMsg ∷ Decode Inbound where decode = Foreign.genericDecode Foreign.defaultOptions


data Topic
  = WorkspaceAttached
  | CompileFinished
  | ServerError

derive instance genericOutbound ∷ Generic Topic _
instance showOutbound ∷ Show Topic where show = Generic.genericShow


data Exception
  = PackageServerUnavailable
  | Unknown String

derive instance genericException ∷ Generic Exception _
instance encodeException ∷ Encode Exception where encode = Foreign.genericEncode Foreign.defaultOptions


newtype Message =
  Message
    { topic ∷ Topic
    , contents ∷ Either Exception Foreign
    }

instance encodeMessage ∷ Encode Message where
  encode (Message value) =
    case value.contents of
      Left e →
        Foreign.toForeign
          { topic: show value.topic
          , exception: Foreign.encode e
          }

      Right a →
        Foreign.toForeign
          { topic: show value.topic
          , contents: a
          }


exception ∷ Topic → Exception → Message
exception topic exception =
  Message
    { topic
    , contents: Left exception
    }


message ∷ ∀ a. Encode a ⇒ Topic → a → Message
message topic value =
  Message
    { topic
    , contents: Right $ Foreign.encode value
    }


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


onConnect ∷ ∀ m. Monad m ⇒ MonadError Error m ⇒ Platform m ⇒ User.Id → m Message
onConnect userId = do
  packagesOrError ← try $ Platform.initialize userId
  case packagesOrError of
    Left error → do
      let errorMessage = Exception.message error
      if String.contains (Pattern "FailedConnectionException2 \"package.elm-lang.org\"") errorMessage
        then pure $ exception WorkspaceAttached PackageServerUnavailable
        else pure $ exception WorkspaceAttached $ Unknown errorMessage
    Right packages →
      pure $ message WorkspaceAttached $ Array.fromFoldable packages


onDisconnect ∷ ∀ m. Monad m ⇒ Platform m ⇒ User.Id → m Unit
onDisconnect userId =
  Platform.destroy userId


onMessage ∷ ∀ m. Monad m ⇒ MonadError Error m ⇒ Platform m ⇒ User.Id → Inbound → m Message
onMessage userId inbound =
  case inbound of
    CompileRequested elm html packages → do
      failureOrErrors ← try $ Platform.compile elm html packages userId
      case failureOrErrors of
        Left failure → 
          pure $ exception CompileFinished $ Unknown $ Exception.message failure
        Right errors →
          pure $ message CompileFinished errors


onError ∷ User.Id → Error → Message
onError userId error =
  exception ServerError $ Unknown $ Exception.message error


connection ∷ ∀ m. Monad m ⇒ Platform m ⇒ UserRepo m ⇒ MonadError Error m ⇒ (m ~> IO) → Connection User.Id
connection runner =
  { onAuthenticate: \request → runner $ onAuthenticate request
  , onDisconnect: \userId → runner $ onDisconnect userId
  , onConnect: \userId send → do
      message ← runner $ onConnect userId
      send $ Foreign.encodeJSON message
  , onMessage: \userId inbound send → do
      message ←
        inbound
          # Foreign.decodeJSON
          # Except.runExcept
          # lmap (\_ → error "Unparseable message")
          # Either.either (onError userId >>> pure) (onMessage userId)
          # runner
      send $ Foreign.encodeJSON message
  , onError: \userId error send →
      let message = onError userId error
      in send $ Foreign.encodeJSON message
  }
