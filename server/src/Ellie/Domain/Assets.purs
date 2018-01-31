module Ellie.Domain.Assets where

import Data.FilePath (FilePath)
import Data.Url (Url)

class Assets m where
  assetUrl ∷ FilePath → m Url
