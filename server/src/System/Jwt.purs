module System.Jwt where

import Prelude
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.IO (IO(..))
import Control.Monad.IO.Effect (INFINITY)

foreign import _encode ∷ { secret ∷ Secret, input ∷ String } → EffFnAff (infinity ∷ INFINITY) Jwt
foreign import _decode ∷ { secret ∷ Secret, input ∷ Jwt } → EffFnAff (infinity ∷ INFINITY) String


newtype Secret =
  Secret String


newtype Jwt =
  Jwt String


encode ∷ Secret → String → IO Jwt
encode secret input = IO $ Aff.fromEffFnAff $ _encode { secret, input }


decode ∷ Secret → Jwt → IO String
decode secret input = IO $ Aff.fromEffFnAff $ _decode { secret, input }
