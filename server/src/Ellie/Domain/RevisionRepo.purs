module Ellie.Domain.RevisionRepo
  ( class RevisionRepo
  , retrieve
  , exists
  , save
  ) where

import Prelude
import Data.Entity (Entity)
import Data.Maybe (Maybe)
import Ellie.Types.Revision (Revision)
import Ellie.Types.Revision as Revision


class RevisionRepo m where
  retrieve :: Revision.Id -> m (Maybe (Entity Revision.Id Revision))
  exists :: Revision.Id -> m Boolean
  save :: Revision.Id → Revision → m Unit
  