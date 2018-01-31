module Ellie.Adapters.CdnAssets where

import Prelude
import Data.FilePath (FilePath, (</>))
import Data.Url (Url)
import Data.Url as Url

type Env r =
  { cdnHost ∷ String
  , assetBase ∷ String
  | r
  }


assetUrl ∷ ∀ r. FilePath → Env r → Url
assetUrl relative env =
  Url.parse $ "https://" <> env.cdnHost </> env.assetBase </> relative