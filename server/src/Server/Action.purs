module Server.Action
  ( ActionT
  , Method(..)
  , class IsParam
  , fromParam
  , class IsBody
  , toBody
  , fromBody
  , makeHandler
  , setStatus
  , setHeader
  , setStringBody
  , setFileBody
  , getBody
  , getHeader
  , getParam
  ) where


import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Aff.Unsafe (unsafeCoerceAff) as Aff
import Control.Monad.IO (IO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader as Reader
import Control.Monad.State (StateT)
import Control.Monad.State as State
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Entity (Entity, class IdentifiedBy)
import Data.Entity as Entity
import Data.FilePath (FilePath)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String as String
import Data.Url (Url)
import Data.Url as Url
import Data.Url.Query as Query
import Data.Uuid (Uuid)
import Data.Uuid as Uuid
import Node.Express.Handler (Handler, HandlerM(..))
import Node.Express.Response as Response
import Node.Express.Types as Express


foreign import _parseRequest ∷
  { parseMethod ∷ String → Method
  , parseUrl ∷ String → Url
  }
  → Express.Request
  → Request


parseMethod ∷ String → Method
parseMethod "GET" = Get
parseMethod "PUT" = Put
parseMethod "POST" = Post
parseMethod "PATCH" = Patch
parseMethod "DELETE" = Delete
parseMethod a = Custom a


data Method
  = Get
  | Put
  | Post
  | Patch
  | Delete
  | Custom String


type Request =
  { method ∷ Method
  , url ∷ Url
  , params ∷ StrMap String
  , headers ∷ StrMap String
  , body ∷ Json
  , vault ∷ StrMap Json
  }


data Content
  = ContentEmpty
  | ContentString String
  | ContentFile FilePath


type Response =
  { status ∷ Int
  , headers ∷ Array { key ∷ String, value ∷ String }
  , content ∷ Content
  }


defaultResponse ∷ Response
defaultResponse =
  { status: 200
  , headers: []
  , content: ContentEmpty
  }


newtype ActionT m a =
  ActionT (ReaderT Request (StateT Response m) a)

derive instance newtypeActionT ∷ Newtype (ActionT m a) _
derive newtype instance functorActionT ∷ Monad m ⇒ Functor (ActionT m)
derive newtype instance applyActionT ∷ Monad m ⇒ Apply (ActionT m)
derive newtype instance bindActionT ∷ Monad m ⇒ Bind (ActionT m)
derive newtype instance applicativeActionT ∷ Monad m ⇒ Applicative (ActionT m)
derive newtype instance monadActionT ∷ Monad m ⇒ Monad (ActionT m)

instance monadTransActionT ∷ MonadTrans ActionT where
  lift = lift >>> lift >>> ActionT


getRequest ∷ ∀ e. HandlerM e Request
getRequest = HandlerM \req _ _ →
  pure $ _parseRequest { parseMethod, parseUrl: Url.parse } req


makeHandler ∷ ∀ m e. Monad m ⇒ (m ~> IO) → ActionT m Unit → Handler e
makeHandler runner (ActionT action) = do
  request ← getRequest
  response ←
    action
      # flip Reader.runReaderT request
      # flip State.execStateT defaultResponse
      # runner
      # unwrap
      # Aff.unsafeCoerceAff
      # Aff.liftAff
  Response.setStatus response.status
  for_ response.headers \{ key, value } →
    Response.setResponseHeader key value
  case response.content of
    ContentEmpty → Response.end
    ContentString str → Response.send str
    ContentFile path → Response.sendFileExt path {} (\_ → pure unit)
  


class IsParam a where
  fromParam ∷ String → Maybe a

instance isParamString ∷ IsParam String where
  fromParam = Just

instance isParamInt ∷ IsParam Int where
  fromParam = Int.fromString

instance isParamUuid ∷ IsParam Uuid where
  fromParam = Uuid.decode


class IsBody a where
  toBody ∷ a → Json
  fromBody ∷ Json → Either Json.Error a

instance isBodyJson ∷ IsBody Json where
  toBody = id
  fromBody = Right

instance isBodyInt ∷ IsBody Int where
  toBody = Json.encodeInt
  fromBody = Json.decodeInt

instance isBodyNumber ∷ IsBody Number where
  toBody = Json.encodeNumber
  fromBody = Json.decodeNumber

instance isBodyString ∷ IsBody String where
  toBody = Json.encodeString
  fromBody = Json.decodeString

instance isBodyBoolean ∷ IsBody Boolean where
  toBody = Json.encodeBoolean
  fromBody = Json.decodeBoolean

instance isBodyUuid ∷ IsBody Uuid where
  toBody = Uuid.encode >>> Json.encodeString
  fromBody value =
    Json.decodeString value >>= \s →
      case Uuid.fromString s of
        Just u → Right u
        Nothing → Left $ Json.Failure "Expecting a UUID" value

instance isBodyMaybe ∷ IsBody a ⇒ IsBody (Maybe a) where
  toBody (Just a) = toBody a
  toBody Nothing = Json.encodeNull
  fromBody value = (Just <$> fromBody value) <|> (Json.decodeNull Nothing value)

instance isBodyArray ∷ IsBody a ⇒ IsBody (Array a) where
  toBody = Json.encodeArray toBody
  fromBody = Json.decodeArray fromBody

instance isBodyEntity ∷ (IsBody k, IsBody r, IdentifiedBy k r) ⇒ IsBody (Entity k r) where
  toBody entity =
    Json.encodeObject
      [ { key: "key", value: toBody $ Entity.key entity }
      , { key: "record", value: toBody $ Entity.record entity }
      ]
  fromBody value =
    Entity.entity
      <$> fromBody value
      <*> fromBody value

getHeader ∷ ∀ m. Monad m ⇒ String → ActionT m (Maybe String)
getHeader key =
  ActionT $ Reader.asks \request →
    StrMap.lookup (String.toLower key) request.headers


getParam ∷ ∀ m a. IsParam a ⇒ Monad m ⇒ String → ActionT m (Maybe a)
getParam key =
  ActionT $ Reader.asks \request →
    let inParams = StrMap.lookup key request.params >>= fromParam
    in case inParams of
      Just value → Just value
      Nothing → Query.get key (Url.query request.url) >>= fromParam


getBody ∷ ∀ m a. Monad m ⇒ IsBody a ⇒ ActionT m (Either Json.Error a)
getBody =
  ActionT $ Reader.asks \request →
    fromBody request.body


setStatus ∷ ∀ m. Monad m ⇒ Int → ActionT m Unit
setStatus status =
  ActionT $ State.modify (_ { status = status })


setHeader ∷ ∀ m. Monad m ⇒ String → String → ActionT m Unit
setHeader key value =
  ActionT $ State.modify \response → response { headers = { key, value } : response.headers }


setStringBody ∷ ∀ m. Monad m ⇒ String → ActionT m Unit
setStringBody content =
  ActionT $ State.modify (_ { content = ContentString content })


setFileBody ∷ ∀ m. Monad m ⇒ FilePath → ActionT m Unit
setFileBody path =
  ActionT $ State.modify (_ { content = ContentFile path })
