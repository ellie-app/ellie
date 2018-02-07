module Server.Socket
  ( Response
  , Auth
  , Connection
  , SocketServer
  , listen
  ) where


import Prelude
import Control.Monad.Aff (runAff_) as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, mkEffFn2, mkEffFn3, runEffFn1, runEffFn2)
import Control.Monad.Except as Except
import Control.Monad.IO (IO)
import Control.Monad.IO as IO
import Control.Monad.IO.Effect (INFINITY)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foreign (Foreign)
import Data.Foreign (toForeign, unsafeFromForeign) as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class (encode) as Foreign
import Data.Foreign.Generic (decodeJSON) as Foreign
import Data.Functor (voidRight)
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Undefined (Undefined, undefined)
import Node.HTTP (Request, Server)

foreign import data SocketServer ∷ Type
foreign import _listen ∷ EffFn2 (infinity ∷ INFINITY) Server RawConnection SocketServer

type IoEffFn0 = Eff (infinity ∷ INFINITY) Undefined
type IoEffFn1 a = EffFn1 (infinity ∷ INFINITY) a Undefined
type IoEffFn2 a b = EffFn2 (infinity ∷ INFINITY) a b Undefined
type IoEffFn3 a b c = EffFn3 (infinity ∷ INFINITY) a b c Undefined


type RawResponse =
  { message ∷ Foreign
  , state ∷ Foreign
  }


type RawConnection =
  { setup ∷ IoEffFn2 Foreign (IoEffFn2 Foreign Foreign)
  , acknowledge ∷ IoEffFn2 Foreign (IoEffFn2 Foreign Foreign)
  , teardown ∷ IoEffFn2 Foreign (IoEffFn1 Foreign)
  , update ∷ IoEffFn3 Foreign String (IoEffFn2 Foreign Foreign)
  , authenticate ∷ IoEffFn2 Request (IoEffFn2 Foreign Foreign)
  , path ∷ String
  }


type Response s o =
  { message ∷ Maybe o
  , state ∷ s
  }


type Auth a =
  { hash ∷ String
  , value ∷ a
  }


type Connection m auth state input output =
  { setup ∷ auth → m state
  , acknowledge ∷ state → m (Response state output)
  , teardown ∷ state → m Unit
  , update ∷ state → input → m (Response state output)
  , authenticate ∷ Request → m (Maybe (Auth auth))
  }


listen ∷ ∀ a s i o m. Decode i ⇒ Encode o ⇒ Server → String → (m ~> IO) → Connection m a s i o → IO SocketServer
listen server path runner connection =
  Eff.liftEff $ runEffFn2 _listen server
    { path: path
    , setup:
        mkEffFn2 $ \identity callback →
          connection.setup (Foreign.unsafeFromForeign identity)
            # runner
            # IO.runIO
            # Aff.runAff_
              (Either.either
                (\error → void $ runEffFn2 callback (Foreign.toForeign error) (Foreign.toForeign undefined))
                (\state → void $ runEffFn2 callback (Foreign.toForeign undefined) (Foreign.toForeign state))
              )
            # voidRight undefined
    , acknowledge:
        mkEffFn2 \state callback →
          connection.acknowledge (Foreign.unsafeFromForeign state)
            # runner
            # IO.runIO
            # Aff.runAff_
              (Either.either
                (\error → void $ runEffFn2 callback (Foreign.toForeign error) (Foreign.toForeign undefined))
                (\result →
                  void $ runEffFn2 callback
                    (Foreign.toForeign undefined)
                    (Foreign.toForeign
                      { message: Maybe.maybe (Foreign.toForeign undefined) Foreign.encode result.message
                      , state: Foreign.toForeign result.state
                      }
                    )
                )
              )
            # voidRight undefined
    , teardown:
        mkEffFn2 \state callback →
          state
            # Foreign.unsafeFromForeign
            # connection.teardown
            # runner
            # IO.runIO
            # Aff.runAff_
              (Either.either
                (\error → void $ runEffFn1 callback (Foreign.toForeign error))
                (\state → void $ runEffFn1 callback (Foreign.toForeign undefined))
              )
            # voidRight undefined
    , update:
        mkEffFn3 \state message callback →
          case Except.runExcept (Foreign.decodeJSON message) of
            Left e →
              voidRight undefined $ runEffFn2 callback
                (Foreign.toForeign (error "Couldn't decode incoming message"))
                (Foreign.toForeign undefined)
            Right incoming →
              incoming
                # connection.update (Foreign.unsafeFromForeign state)
                # runner
                # IO.runIO
                # Aff.runAff_
                  (Either.either
                    (\error → void $ runEffFn2 callback (Foreign.toForeign error) (Foreign.toForeign undefined))
                    (\result →
                      void $ runEffFn2 callback
                        (Foreign.toForeign undefined)
                        (Foreign.toForeign
                          { message: Maybe.maybe (Foreign.toForeign undefined) Foreign.encode result.message
                          , state: Foreign.toForeign result.state
                          }
                        )
                    )
                  )
                # voidRight undefined
    , authenticate:
        mkEffFn2 \request callback →
          connection.authenticate request
            # runner
            # IO.runIO
            # Aff.runAff_
              (Either.either
                (\error → void $ runEffFn2 callback (Foreign.toForeign error) (Foreign.toForeign undefined))
                (\auth →
                    void $ runEffFn2 callback
                      (Foreign.toForeign undefined) 
                      (Maybe.maybe (Foreign.toForeign undefined) Foreign.toForeign auth)
                )
              )
            # voidRight undefined
    }
