module Data.Maybe.Extra where

import Data.Maybe (Maybe(..))

guard :: ∀ a. Boolean -> Maybe a -> Maybe a
guard true maybe = maybe
guard false _ = Nothing