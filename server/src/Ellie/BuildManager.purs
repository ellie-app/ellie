module Ellie.BuildManager where

import Prelude

import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Error.Class (class MonadError, try)
import Data.Either (Either(..))
import Data.Entity (Entity)
import Data.Entity as Entity
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (genericDecode, genericEncode, defaultOptions) as Foreign
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (over, unwrap)
import Data.String.Class (toString) as String
import Data.UniqueId (UniqueId(..))
import Data.Url as Url
import Data.Url.Query as Query
import Ellie.Domain.Platform (class Platform)
import Ellie.Domain.Platform as Platform
import Ellie.Domain.UserRepo (class UserRepo)
import Ellie.Domain.UserRepo as UserRepo
import Ellie.Elm.Compiler.Error as Compiler
import Ellie.Elm.Package (Package)
import Ellie.Types.User (User)
import Ellie.Types.User as User
import Ellie.Types.Workspace (Workspace(..))
import Node.HTTP (Request)
import Node.HTTP as HTTP
import Server.Socket (Connection, Response, Auth)
import System.Jwt (Jwt(..))
import Debug as Debug


data Inbound
  = AttachToWorkspace
  | CompileRequested String String (Array Package)

derive instance genericInbound ∷ Generic Inbound _
instance decodeMsg ∷ Decode Inbound where decode = Foreign.genericDecode Foreign.defaultOptions


data Outbound
  = WorkspaceAttached (Array Package)
  | CompileSucceeded (Array Compiler.Error)
  | CompileFailed String
  | SaveSucceeded
derive instance genericOutbound ∷ Generic Outbound _
instance encodeOutbound ∷ Encode Outbound where encode = Foreign.genericEncode Foreign.defaultOptions


type State =
  { user ∷ Entity User.Id User
  , workspace ∷ Workspace
  }


authenticate ∷ ∀ m. UserRepo m ⇒ Monad m ⇒ Request → m (Maybe (Auth (Entity User.Id User)))
authenticate request =
  let url = Url.parse $ HTTP.requestURL request
  in case Query.get "token" (Url.query url) of
    Just token → do
      maybeUserId ← UserRepo.verify (Jwt token)
      maybeEntity ← Maybe.maybe (pure Nothing) UserRepo.retrieve maybeUserId
      case maybeEntity of
        Just entity → pure $ Just { hash: String.toString $ Entity.key entity, value: entity }
        Nothing → pure Nothing
    Nothing →
      pure Nothing


setup ∷ ∀ m. Platform m ⇒ Functor m ⇒ Entity User.Id User → m State
setup user =
  { user, workspace: _ } <$> Platform.initialize


acknowledge ∷ ∀ m. Applicative m ⇒ State → m (Response State Outbound)
acknowledge state =
  pure $ { state, message: Just $ WorkspaceAttached $ _.packages $ unwrap $ state.workspace }


teardown ∷ ∀ m. Platform m ⇒ State → m Unit
teardown state =
  Platform.destroy state.workspace


update ∷
  ∀ m.
  Monad m ⇒ MonadError Error m ⇒ Platform m ⇒
  State → Inbound → m (Response State Outbound)
update state inbound =
  case inbound of
    AttachToWorkspace →
      pure
        { message: Just $ WorkspaceAttached $ _.packages $ unwrap $ state.workspace
        , state
        }

    CompileRequested elm html packages → do
      let
        updatedWorkspace =
          over Workspace (_ { packages = packages }) state.workspace

      failureOrErrors ←
        try $ Platform.compile elm html updatedWorkspace

      let
        message =
          case failureOrErrors of
            Left failure → CompileFailed $ Exception.message failure
            Right errors → CompileSucceeded errors

      pure
        { message: Just message
        , state: state { workspace = updatedWorkspace }
        }
      

connection ∷
  ∀ m.
  Monad m ⇒ Platform m ⇒ UserRepo m ⇒ MonadError Error m ⇒
  Connection m (Entity User.Id User) State Inbound Outbound
connection =
  { setup
  , acknowledge
  , teardown
  , update
  , authenticate
  }