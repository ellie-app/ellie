module System.Cache where

import Prelude

import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.IO (IO)
import Data.Maybe (Maybe(..))
import System.Time as Time


newtype Cache a =
  Cache { ttl ∷ Int, ref ∷ Ref (Maybe { value ∷ a, lastFilled ∷ Int }) }


create ∷ ∀ e a. Int → IO (Cache a)
create ttl = 
  Eff.liftEff $
    Ref.newRef Nothing <#> \ref →
      Cache { ttl, ref }


read ∷ ∀ e a. IO a → Cache a → IO a
read reload cache@(Cache { ttl, ref }) = do
  current ←
    Eff.liftEff $
      Ref.readRef ref >>= \currentRef →
        case currentRef of
          Just { value, lastFilled } → do
            now ← Time.now
            if now - lastFilled < ttl
              then pure $ Just value
              else pure Nothing
          _ →
            pure Nothing
  case current of
    Just value →
      pure value
    Nothing -> do
      newValue ← reload
      write newValue cache
      pure newValue


write ∷ ∀ e a. a → Cache a → IO Unit
write value (Cache { ttl, ref }) =
  Eff.liftEff $
    Time.now >>= \now →
      Ref.writeRef ref $ Just { value, lastFilled: now }
