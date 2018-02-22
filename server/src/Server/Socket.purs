module Server.Socket
  ( Connection
  , SocketServer
  , listen
  ) where


import Prelude

import Control.Monad.Aff (launchAff_) as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn1, mkEffFn2, mkEffFn3, runEffFn1, runEffFn3)
import Control.Monad.IO (IO)
import Control.Monad.IO as IO
import Control.Monad.IO.Effect (INFINITY)
import Data.Foreign (Foreign)
import Data.Foreign (unsafeFromForeign, toForeign) as Foreign
import Data.Maybe (Maybe(..))
import Data.Undefined (undefined)
import Node.HTTP (Request, Server)


foreign import data SocketServer ∷ Type
foreign import _listen ∷ EffFn3 (infinity ∷ INFINITY) Unit Server RawConnection SocketServer

type IoEff a = Eff (infinity ∷ INFINITY) a
type IoEffFn1 a b = EffFn1 (infinity ∷ INFINITY) a b
type IoEffFn2 a b c = EffFn2 (infinity ∷ INFINITY) a b c
type IoEffFn3 a b c d = EffFn3 (infinity ∷ INFINITY) a b c d


type RawConnection =
  { onAuthenticate ∷ IoEffFn2 Request (IoEffFn1 Foreign Unit) Unit
  , onConnect ∷ IoEffFn2 Foreign (IoEffFn1 String Unit) Unit
  , onMessage ∷ IoEffFn3 Foreign String (IoEffFn1 String Unit) Unit
  , onDisconnect ∷ IoEffFn1 Foreign Unit
  , path ∷ String
  }


type Connection a =
  { onAuthenticate ∷ Request → IO (Maybe a)
  , onConnect ∷ a → (String → IO Unit) → IO Unit
  , onMessage ∷ a → String → (String → IO Unit) → IO Unit
  , onDisconnect ∷ a → IO Unit
  , onError ∷ a → Error → (String → IO Unit) → IO Unit
  }


listen ∷ ∀ a. Server → String → Connection a → IO SocketServer
listen server path connection =
  Eff.liftEff $ runEffFn3 _listen unit server
    { path: path
    , onConnect: mkEffFn2 \identityForeign sendEffFn →
        let
          send message = Eff.liftEff $ runEffFn1 sendEffFn message
          identity = Foreign.unsafeFromForeign identityForeign
        in
          Aff.launchAff_ $ IO.runIO $ connection.onConnect identity send
    , onMessage: mkEffFn3 \identityForeign inmessage sendEffFn →
        let
          send outmessage = Eff.liftEff $ runEffFn1 sendEffFn outmessage
          identity = Foreign.unsafeFromForeign identityForeign
        in
          Aff.launchAff_ $ IO.runIO $ connection.onMessage identity inmessage send
    , onDisconnect: mkEffFn1 \identityForeign →
        let identity = Foreign.unsafeFromForeign identityForeign
        in Aff.launchAff_ $ IO.runIO $ connection.onDisconnect identity
    , onAuthenticate: mkEffFn2 \request callback → Aff.launchAff_ $ IO.runIO do
        maybeIdentity ← connection.onAuthenticate request
        case maybeIdentity of
          Just identity →
            Eff.liftEff $ runEffFn1 callback $ Foreign.toForeign identity
          Nothing →
            Eff.liftEff $ runEffFn1 callback $ Foreign.toForeign undefined
    }
