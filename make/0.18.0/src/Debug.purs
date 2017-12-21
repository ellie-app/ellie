module Debug (log, break) where

import Prelude

foreign import _log :: ∀ a. a -> a
foreign import _break :: ∀ a. a -> a


log :: ∀ a. a -> a
log = _log


break :: ∀ a. a -> a
break = _break