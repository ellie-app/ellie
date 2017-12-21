module Ellie.Prelude
  ( module Prelude
  , (|>)
  , (<|)
  ) where

import Prelude as Prelude
import Data.Function (apply, applyFlipped)

infixl 1 applyFlipped as |>
infixr 0 apply as <|
