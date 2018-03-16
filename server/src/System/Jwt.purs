module System.Jwt where

import Prelude
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.IO (IO(..))
import Control.Monad.IO.Effect (INFINITY)
import Server.Action (class IsParam, class IsBody)

foreign import _encode ∷ { secret ∷ Secret, input ∷ String } → EffFnAff (infinity ∷ INFINITY) Jwt
foreign import _decode ∷ { secret ∷ Secret, input ∷ Jwt } → EffFnAff (infinity ∷ INFINITY) String


newtype Secret =
  Secret String


newtype Jwt =
  Jwt String

derive newtype instance isParamJwt ∷ IsParam Jwt
derive newtype instance isBodyJwt ∷ IsBody Jwt

encode ∷ Secret → String → IO Jwt
encode secret input = IO $ Aff.fromEffFnAff $ _encode { secret, input }


decode ∷ Secret → Jwt → IO String
decode secret input = IO $ Aff.fromEffFnAff $ _decode { secret, input }
