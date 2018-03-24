module Ellie.Types.User
  ( Id(..)
  , idToPostgres
  , idFromPostgres
  , idToBody
  , idFromBody
  , User(..)
  , default
  , toBody
  , fromBody
  , toPostgres
  , fromPostgres
  , entityToPostgres
  , entityFromPostgres
  , entityToBody
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Entity (class IdentifiedBy, Entity)
import Data.Entity as Entity
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Uuid (Uuid)
import Data.Uuid as Uuid
import Ellie.Types.Settings (Settings)
import Ellie.Types.Settings as Settings
import Ellie.Types.TermsVersion (TermsVersion)
import Ellie.Types.TermsVersion as TermsVersion


-- ID


newtype Id
  = Id Uuid

derive instance newtypeId ∷ Newtype Id _
derive newtype instance eqId ∷ Eq Id
derive newtype instance ordId ∷ Ord Id


idToPostgres ∷ Id → Array Json.KeyValue
idToPostgres (Id uuid) =
  [ { key: "id", value: Json.encodeString $ Uuid.toString uuid } ]


idFromPostgres ∷ Json → Either Json.Error Id
idFromPostgres value =
  Json.decodeAtField "id" value decodeUuid
    <#> Id
  where
    decodeUuid ∷ Json → Either Json.Error Uuid
    decodeUuid value =
      Json.decodeString value >>= \string →
        case Uuid.fromString string of
          Just uuid → Right uuid
          Nothing → Left (Json.Failure "Expecting a UUID" value)



idToBody ∷ Id → Json
idToBody (Id id) =
  Json.encodeString $ Uuid.base49Encode id


idFromBody ∷ Json → Either Json.Error Id
idFromBody value =
  Json.decodeString value >>= \string →
    case Uuid.base49Decode string of
      Just uuid → Right $ Id uuid
      Nothing → Left $ Json.Failure "Expecting a base49 encoded UUID" value


-- USER


newtype User =
  User
    { termsVersion ∷ Maybe TermsVersion
    , settings ∷ Settings
    }

derive instance newtypeUser ∷ Newtype User _


default ∷ User
default =
  User
    { termsVersion: Nothing
    , settings: Settings.default
    }


toBody ∷ User → Json
toBody (User user) =
  Json.encodeObject
    [ { key: "settings", value: Settings.toBody user.settings }
    , { key: "termsVersion", value: Json.encodeNullable TermsVersion.toJson user.termsVersion }
    ]


fromBody ∷ Json → Either Json.Error User
fromBody value =
  { termsVersion: _, settings: _ }
    <$> Json.decodeAtField "termsVersion" value (Json.decodeNullable TermsVersion.fromJson)
    <*> Json.decodeAtField "settings" value Settings.fromBody
    <#> User


fromPostgres ∷ Json → Either Json.Error User
fromPostgres value =
  { termsVersion: _, settings: _ }
    <$> Json.decodeAtField "terms_version" value (Json.decodeNullable TermsVersion.fromJson)
    <*> Json.decodeAtField "settings" value Settings.fromPostgres
    <#> User


toPostgres ∷ User → Array Json.KeyValue
toPostgres (User user) =
  [ { key: "settings", value: Settings.toPostgres user.settings }
  , { key: "terms_version", value: Json.encodeNullable TermsVersion.toJson user.termsVersion }
  ]


 -- ENTITY


instance identifiedByIdUser ∷ IdentifiedBy Id User


entityToPostgres ∷ Entity Id User → Array Json.KeyValue
entityToPostgres entity =
  idToPostgres (Entity.key entity) <> toPostgres (Entity.record entity)


entityFromPostgres ∷ Json → Either Json.Error (Entity Id User)
entityFromPostgres value =
  Entity.entity
    <$> idFromPostgres value
    <*> fromPostgres value

entityToBody ∷ Entity Id User → Json
entityToBody entity =
  Json.encodeObject
    [ { key: "key", value: idToBody $ Entity.key entity }
    , { key: "record", value: toBody $ Entity.record entity }
    ]
