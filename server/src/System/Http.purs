module System.Http where

import Prelude
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Aff.Class (liftAff) as Aff
import Control.Monad.Eff (kind Effect)
import Control.Monad.IO (IO)
import Data.FilePath (FilePath)
import Data.Json (Json)


foreign import data HTTP :: Effect
foreign import _get ∷ ∀ e. String → EffFnAff (http ∷ HTTP | e) Json
foreign import _download ∷ ∀ e. Unit → String → FilePath → EffFnAff (http ∷ HTTP | e) Unit


get ∷ ∀ e. String → IO Json
get url = Aff.liftAff $ Aff.fromEffFnAff $ _get url 


download ∷ ∀ e. String → FilePath → IO Unit
download url target = Aff.liftAff $ Aff.fromEffFnAff $ _download unit url target
