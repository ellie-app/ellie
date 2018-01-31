module System.UniqueId
  ( uniqueId
  ) where

import Prelude
import Control.Monad.Eff.Class as Eff
import Control.Monad.IO (IO)
import Data.UniqueId (UniqueId(..))
import Data.UUID as UUID

uniqueId :: IO UniqueId
uniqueId =
   Eff.liftEff $ (UniqueId <$> show <$> UUID.genUUID)