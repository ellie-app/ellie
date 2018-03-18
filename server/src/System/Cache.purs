module System.Cache where

import Prelude

import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.IO (IO)
import Data.Maybe (Maybe(..))
import Data.Time.Good (Posix)
import Data.Time.Good (Span, diff) as Time
import System.Time (now) as Time


newtype Cache a =
  Cache { ttl ∷ Time.Span, ref ∷ Ref (Maybe { value ∷ a, lastFilled ∷ Posix }) }


create ∷ ∀ e a. Time.Span → IO (Cache a)
create ttl = 
  Eff.liftEff $
    Ref.newRef Nothing <#> \ref →
      Cache { ttl, ref }


read ∷ ∀ e a. IO a → Cache a → IO a
read reload cache@(Cache { ttl, ref }) = do
  current ←
    Eff.liftEff (Ref.readRef ref) >>= \currentRef →
      case currentRef of
        Just { value, lastFilled } → do
          now ← Time.now
          if Time.diff now lastFilled == ttl
            then pure $ Just value
            else pure Nothing
        _ →
          pure Nothing
  case current of
    Just value →
      pure value
    Nothing → do
      newValue ← reload
      write newValue cache
      pure newValue


write ∷ ∀ e a. a → Cache a → IO Unit
write value (Cache { ttl, ref }) =
  Time.now >>= \now →
    Eff.liftEff $ Ref.writeRef ref $ Just { value, lastFilled: now }
