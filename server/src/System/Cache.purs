module System.Cache where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Ref as Ref
import System.Time as Time
import Control.Monad.Eff.Unsafe as Eff
import Data.Maybe (Maybe(..))


foreign import data CACHE :: Effect


newtype Cache a =
  Cache { ttl :: Int, ref :: Ref (Maybe { value :: a, lastFilled :: Int }) }


create :: ∀ e a. Int -> Eff (cache :: CACHE | e) (Cache a)
create ttl = 
  Eff.unsafeCoerceEff $
    Ref.newRef Nothing <#> \ref ->
      Cache { ttl, ref }


read :: ∀ e a. Cache a -> Eff (cache :: CACHE | e) (Maybe a)
read (Cache { ttl, ref }) =
  Eff.unsafeCoerceEff $
    Ref.readRef ref >>= \currentRef ->
      case currentRef of
        Just { value, lastFilled } -> do
          now <- Time.now
          if now - lastFilled < ttl
            then pure $ Just value
            else pure Nothing
        _ ->
          pure Nothing


write :: ∀ e a. a -> Cache a -> Eff (cache ::CACHE | e) Unit
write value (Cache { ttl, ref }) =
  Eff.unsafeCoerceEff $
    Time.now >>= \now ->
      Ref.writeRef ref $ Just { value, lastFilled: now }