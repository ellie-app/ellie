module Ellie.Domain.UserRepo
  ( class UserRepo
  , retrieve
  , create
  , save
  , verify
  , sign
  ) where

import Prelude
import Data.Entity (Entity)
import Data.Maybe (Maybe)
import Ellie.Types.User (User)
import Ellie.Types.User as User
import System.Jwt (Jwt)


class UserRepo m where
  retrieve :: User.Id -> m (Maybe (Entity User.Id User))
  create ∷ m (Entity User.Id User)
  save ∷ User.Id → User → m Unit
  verify ∷ Jwt → m (Maybe User.Id)
  sign ∷ User.Id → m Jwt
  