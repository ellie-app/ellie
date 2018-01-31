module Ellie.Types.TermsVersion where

import Prelude
import Data.Foreign.Class (class Decode, class Encode)

newtype TermsVersion =
  TermsVersion Int

latest ∷ TermsVersion
latest =
  TermsVersion 1

derive newtype instance showTermsVersion ∷ Show TermsVersion
derive newtype instance eqTermsVersion :: Eq TermsVersion
derive newtype instance ordTermsVersion :: Ord TermsVersion
derive newtype instance decodeTermsVersion :: Decode TermsVersion
derive newtype instance encodeTermsVersion :: Encode TermsVersion