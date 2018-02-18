module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class as Eff
import Control.Monad.Eff.Ref as Ref
import Control.Monad.IO (IO)
import Control.Monad.IO (launchIO) as IO
import Control.Monad.IO.Effect (INFINITY)
import Control.Monad.IOSync (runIOSync) as IO
import Data.Function.Uncurried (Fn3)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Ellie (class Ellie, DevEnv(DevEnv))
import Ellie as Ellie
import Ellie.Api as Api
import Ellie.BuildManager as BuildManager
import Node.Express.App (App)
import Node.Express.App (get, listenHttp, post, useExternal) as Express
import Node.Express.Handler (Handler)
import Node.Express.Types (ExpressM, Request, Response) as Express
import Node.HTTP (Server)
import Node.Process as Process
import Server.Action (ActionT)
import Server.Action as Action
import Server.Socket as Socket
import System.Jwt (Secret(..))


foreign import jsonBodyParser ∷ ∀ e.
  Fn3
    Express.Request
    Express.Response
    (Express.ExpressM e Unit)
    (Express.ExpressM e Unit)


routes ∷ ∀ m e. Ellie m ⇒ (ActionT m Unit → Handler e) → App e
routes makeHandler = do
  Express.useExternal jsonBodyParser
  Express.get  "/"                                                $ makeHandler Api.newUi
  Express.get  "/new"                                             $ makeHandler Api.newUi
  Express.get  "/private-api/revision/:projectId/:revisionNumber" $ makeHandler Api.getRevision
  Express.get  "/private-api/packages/search"                     $ makeHandler Api.searchPackages
  Express.post "/private-api/format"                              $ makeHandler Api.formatCode
  Express.post "/private-api/me"                                  $ makeHandler Api.me
  Express.post "/private-api/me/settings"                         $ makeHandler Api.saveSettings


setup ∷ IO Server
setup = do
  port ← Maybe.fromMaybe 1337 <$> (_ >>= Int.fromString) <$> (Eff.liftEff (Process.lookupEnv "PORT"))
  index ← Eff.liftEff $ Ref.newRef Nothing
  let jwtSecret = Secret "abc123"
  let env = DevEnv { index, jwtSecret, assetBase: "", webpackHost: "localhost:1338" }
  server ← Eff.liftEff $ Express.listenHttp (routes (Action.makeHandler (Ellie.runEllieM env))) port (\_ → pure unit)
  _ ← Socket.listen server "/workspace" (Ellie.runEllieM env) BuildManager.connection
  pure server


main ∷ Eff (infinity ∷ INFINITY) Unit
main = IO.runIOSync $ IO.launchIO $ do
  server ← setup
  pure unit
