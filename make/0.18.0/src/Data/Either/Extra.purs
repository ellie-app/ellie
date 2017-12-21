module Data.Either.Extra where

import Data.Either (Either(..))


withDefault :: ∀ a b. b -> Either a b -> b
withDefault default (Left _) = default
withDefault _ (Right b) = b
