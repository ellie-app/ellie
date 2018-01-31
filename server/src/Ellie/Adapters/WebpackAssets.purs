module Ellie.Adapters.WebpackAssets where

import Prelude
import Data.FilePath (FilePath, (</>))
import Data.Url (Url)
import Data.Url as Url

type Env r =
  { webpackHost ∷ String
  , assetBase ∷ String
  | r
  }


assetUrl ∷ ∀ r. FilePath → Env r → Url
assetUrl relative env =
  Url.parse $ "http://" <> env.webpackHost </> env.assetBase </> relative