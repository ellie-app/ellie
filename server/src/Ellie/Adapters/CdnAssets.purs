module Ellie.Adapters.CdnAssets where

import Prelude

import Control.Monad.IO.Class (class MonadIO)
import Control.Monad.IO.Class as IO
import Data.FilePath (FilePath, (<.>), (</>))
import Data.Maybe (Maybe(..))
import Data.Url (Url)
import Data.Url as Url
import Ellie.Types.TermsVersion (TermsVersion)
import Ellie.Types.TermsVersion as TermsVersion
import System.FileSystem as FileSystem

type Env r =
  { cdnHost ∷ String
  , assetBase ∷ String
  | r
  }


assetUrl ∷ ∀ r. FilePath → Env r → Url
assetUrl relative env =
  Url.parse $ "https://" <> env.cdnHost </> env.assetBase </> relative


termsHtml ∷ ∀ m. MonadIO m ⇒ TermsVersion → m (Maybe FilePath)
termsHtml termsVersion = IO.liftIO do
  if termsVersion > TermsVersion.latest || termsVersion < TermsVersion.first
    then pure Nothing
    else do
      cwd ← FileSystem.cwd
      pure $ Just $ cwd </> "static/terms" </> show termsVersion <.> "html"
