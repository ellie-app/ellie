module System.Time where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.IO (IO)
import Control.Monad.IO.Effect (INFINITY)
import Data.Time.Good (Posix)
import Data.Time.Good as Time

foreign import _now ∷ Eff (time ∷ INFINITY) Int


now ∷ IO Posix
now = Time.millisToPosix <$> Eff.liftEff _now
