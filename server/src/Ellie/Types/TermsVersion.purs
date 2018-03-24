module Ellie.Types.TermsVersion where

import Prelude

import Data.Either (Either)
import Data.Int as Int
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe)


newtype TermsVersion =
  TermsVersion Int

derive newtype instance showTermsVersion ∷ Show TermsVersion
derive newtype instance eqTermsVersion ∷ Eq TermsVersion
derive newtype instance ordTermsVersion ∷ Ord TermsVersion


latest ∷ TermsVersion
latest =
  TermsVersion 1


first ∷ TermsVersion
first =
  TermsVersion 1


toJson ∷ TermsVersion → Json
toJson (TermsVersion i) = Json.encodeInt i


fromJson ∷ Json → Either Json.Error TermsVersion
fromJson value =
  TermsVersion <$> Json.decodeInt value


fromString ∷ String → Maybe TermsVersion
fromString input =
  TermsVersion <$> Int.fromString input
