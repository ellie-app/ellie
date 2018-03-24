module Ellie.Domain.Assets where

import Data.FilePath (FilePath)
import Data.Maybe (Maybe)
import Data.Url (Url)
import Ellie.Types.TermsVersion (TermsVersion)


class Assets m where
  assetUrl ∷ FilePath → m Url
  termsHtml ∷ TermsVersion → m (Maybe FilePath)
