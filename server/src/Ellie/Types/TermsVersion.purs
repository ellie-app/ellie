module Ellie.Types.TermsVersion where

import Prelude
import Server.Action (class IsBody)
import System.Postgres (class FromResult, class ToValue)

newtype TermsVersion =
  TermsVersion Int

latest ∷ TermsVersion
latest =
  TermsVersion 1

derive newtype instance showTermsVersion ∷ Show TermsVersion
derive newtype instance eqTermsVersion ∷ Eq TermsVersion
derive newtype instance ordTermsVersion ∷ Ord TermsVersion
derive newtype instance fromResultTermsVersion ∷ FromResult TermsVersion
derive newtype instance toValueTermsVersion ∷ ToValue TermsVersion
derive newtype instance isBodyTermsVersion ∷ IsBody TermsVersion
