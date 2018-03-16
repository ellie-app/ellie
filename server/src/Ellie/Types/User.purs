module Ellie.Types.User
  ( User(..)
  , Id(..)
  , default
  -- newtypeId
  -- toStringId
  -- toJsonId
  -- eqId
  -- ordId
  -- newtypeUser
  -- identifiedByIdUser
  ) where

import Prelude

import Data.Entity (class IdentifiedBy)
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Class (class ToString)
import Data.Uuid (Uuid)
import Ellie.Types.Settings (Settings)
import Ellie.Types.Settings as Settings
import Server.Action (class IsBody)
import Server.Action as Action
import System.Postgres (class FromResult, class ToArguments)
import System.Postgres as Postgres

newtype Id
  = Id Uuid


newtype User =
  User
    { termsVersion ∷ Maybe Int
    , settings ∷ Settings
    }


default ∷ User
default =
  User
    { termsVersion: Nothing
    , settings: Settings.default
    }


derive instance newtypeId ∷ Newtype Id _
derive newtype instance toStringId ∷ ToString Id
derive newtype instance eqId ∷ Eq Id
derive newtype instance ordId ∷ Ord Id
derive newtype instance isBodyId ∷ IsBody Id

instance toArgumentsId ∷ ToArguments Id where
  toArguments (Id id) =
    [ { key: "id", value: Postgres.toValue id } ]

instance fromResultId ∷ FromResult Id where
  fromResult value =
    Json.decodeAtField "id" value Postgres.fromResult
      <#> Id

derive instance newtypeUser ∷ Newtype User _
instance identifiedByIdUser ∷ IdentifiedBy Id User

instance isBodyUser ∷ IsBody User where
  toBody (User user) =
    Json.encodeObject
      [ { key: "settings", value: Action.toBody user.settings }
      , { key: "termsVersion", value: Action.toBody user.termsVersion }
      ]
  fromBody value =
    { termsVersion: _, settings: _ }
      <$> Json.decodeAtField "termsVersion" value Action.fromBody
      <*> Json.decodeAtField "settings" value Action.fromBody
      <#> User

instance fromResultUser ∷ FromResult User where
  fromResult value =
    { termsVersion: _, settings: _ }
      <$> Json.decodeAtField "terms_version" value Postgres.fromResult
      <*> Json.decodeAtField "settings" value Postgres.fromResult
      <#> User

instance toArgumentsUser ∷ ToArguments User where
  toArguments (User u) =
    [ { key: "settings", value: Postgres.toValue u.settings }
    , { key: "terms_version", value: Postgres.toValue u.termsVersion }
    ]
