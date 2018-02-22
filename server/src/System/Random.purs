module System.Random where

import Prelude
import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.Eff.Random as Random
import Control.Monad.IO (IO)


int ∷ IO Int
int =
  Eff.liftEff $ Random.randomInt 0 2147483647
