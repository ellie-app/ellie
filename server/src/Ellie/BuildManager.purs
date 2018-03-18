module Ellie.BuildManager where

import Prelude

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Error.Class (class MonadError, try)
import Control.Monad.IO (IO)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Entity as Entity
import Data.Json (Json)
import Data.Json as Json
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
import Elm.Compiler.Error as Compiler
import Elm.Package (Package)
import Elm.Package as Package
import Node.HTTP (Request)
import Node.HTTP as HTTP
import Server.Socket (Connection)
import System.Jwt (Jwt(..))


data Inbound
  = CompileRequested String String (Array Package)

decodeInbound ∷ Json → Either Json.Error Inbound
decodeInbound value = do
  tag ← Json.decodeAtField "tag" value Json.decodeString
  case tag of
    "CompileRequested" →
      CompileRequested
        <$> Json.decodeAtPath [ "contents", "0" ] value Json.decodeString
        <*> Json.decodeAtPath [ "contents", "1" ] value Json.decodeString
        <*> Json.decodeAtPath [ "contents", "2" ] value (Json.decodeArray Package.fromBody)
    _ →
      Left $ Json.Failure ("Unrecognized tag " <> tag) value


data Topic
  = WorkspaceAttached
  | CompileFinished
  | ServerError

instance showTopic ∷ Show Topic where
  show WorkspaceAttached = "WorkspaceAttached"
  show CompileFinished = "CompileFinished"
  show ServerError = "ServerError"


data Exception
  = PackageServerUnavailable
  | Unknown String

encodeException ∷ Exception → Json
encodeException exception =
  case exception of
    PackageServerUnavailable →
      Json.encodeObject
        [ { key: "tag", value: Json.encodeString "PackageServerUnavailable" } ]
    Unknown message →
      Json.encodeObject
        [ { key: "tag", value: Json.encodeString "Unknown" }
        , { key: "contents", value: Json.encodeString message }
        ]


newtype Message =
  Message
    { topic ∷ Topic
    , contents ∷ Either Exception Json
    }

encodeMessage ∷ Message → Json
encodeMessage (Message value) =
  case value.contents of
    Left e →
      Json.encodeObject
        [ { key: "topic", value: Json.encodeString (show value.topic) }
        , { key: "exception", value: encodeException e }
        ]
    Right a →
      Json.encodeObject
        [ { key: "topic", value: Json.encodeString (show value.topic) }
        , { key: "contents", value: a }
        ]


exception ∷ Topic → Exception → Message
exception topic exception =
  Message
    { topic
    , contents: Left exception
    }


message ∷ Topic → Json → Message
message topic value =
  Message
    { topic
    , contents: Right $ value
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
      pure $ message WorkspaceAttached $ Json.encodeArray Package.toBody $ Array.fromFoldable packages


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
          pure $ message CompileFinished $ Json.encodeArray Compiler.encode errors


onError ∷ User.Id → Error → Message
onError userId error =
  exception ServerError $ Unknown $ Exception.message error


connection ∷ ∀ m. Monad m ⇒ Platform m ⇒ UserRepo m ⇒ MonadError Error m ⇒ (m ~> IO) → Connection User.Id
connection runner =
  { onAuthenticate: \request → runner $ onAuthenticate request
  , onDisconnect: \userId → runner $ onDisconnect userId
  , onConnect: \userId send → do
      message ← runner $ onConnect userId
      send $ Json.stringify $ encodeMessage message
  , onMessage: \userId inbound send → do
      message ←
        inbound
          # Json.parse
          >>= decodeInbound
          # lmap (Json.errorToString >>> error)
          # Either.either (onError userId >>> pure) (onMessage userId)
          # runner
      send $ Json.stringify $ encodeMessage message
  , onError: \userId error send →
      let message = onError userId error
      in send $ Json.stringify $ encodeMessage message
  }
