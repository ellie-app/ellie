module Ellie.Types.Revision
  ( ProjectId
  , projectIdToPostgres
  , projectIdFromString
  , Id(..)
  , idToString
  , idFromPostgres
  , idToPostgres
  , idToBody
  , idFromBody
  , Revision(..)
  , toBody
  , fromBody
  , toPostgres
  , fromPostgres
  , entityToPostgres
  , entityFromPostgres
  , entityToBody
  , entityFromBody
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Entity (class IdentifiedBy, Entity)
import Data.Entity as Entity
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Class (toString) as String
import Data.Uuid (Uuid)
import Data.Uuid as Uuid
import Debug.Trace as Debug
import Ellie.Types.TermsVersion (TermsVersion)
import Ellie.Types.TermsVersion as TermsVersion
import Ellie.Types.User as User
import Elm.Package (Package)
import Elm.Package as Package
import Elm.Version (Version)
import Elm.Version as Version


-- PROJECT ID


newtype ProjectId =
  ProjectId Uuid

derive instance newtypeProjectId ∷ Newtype ProjectId _

projectIdFromString ∷ String → Maybe ProjectId
projectIdFromString input =
  ProjectId <$> Uuid.base49Decode input


projectIdToBody ∷ ProjectId → Json
projectIdToBody (ProjectId uuid) =
  Json.encodeString $ Uuid.base49Encode uuid


projectIdFromBody ∷ Json → Either Json.Error ProjectId
projectIdFromBody value =
  Json.decodeString value >>= \string →
    case Uuid.base49Decode string of
      Just uuid → Right (ProjectId uuid)
      Nothing → Left (Json.Failure "Expecting a UUID" value)


projectIdToPostgres ∷ ProjectId → Json
projectIdToPostgres (ProjectId uuid) =
  Json.encodeString $ Uuid.toString uuid


projectIdFromPostgres ∷ Json → Either Json.Error ProjectId
projectIdFromPostgres value =
  Json.decodeString value >>= \string →
    case Uuid.fromString string of
      Just uuid → Right (ProjectId uuid)
      Nothing → Left (Json.Failure "Expecting a UUID" value)


-- ID

newtype Id =
  Id { projectId ∷ ProjectId, revisionNumber ∷ Int }

derive instance newtypeId ∷ Newtype Id _


idToString ∷ Id → String
idToString (Id { projectId: (ProjectId projectId), revisionNumber }) =
    Uuid.base49Encode projectId <> "/" <> String.toString revisionNumber


idFromPostgres ∷ Json → Either Json.Error Id
idFromPostgres value =
  { projectId: _, revisionNumber: _ }
    <$> Json.decodeAtField "project_id" value projectIdFromPostgres
    <*> Json.decodeAtField "revision_number" value Json.decodeInt
    <#> Id


idToPostgres ∷ Id → Array Json.KeyValue
idToPostgres (Id { projectId, revisionNumber }) =
  [ { key: "project_id", value: projectIdToPostgres projectId }
  , { key: "revision_number", value: Json.encodeInt revisionNumber }
  ]


idToBody ∷ Id → Json
idToBody (Id {projectId, revisionNumber }) =
    Json.encodeObject
      [ { key: "projectId", value: projectIdToBody projectId }
      , { key: "revisionNumber", value: Json.encodeInt revisionNumber }
      ]

idFromBody ∷ Json → Either Json.Error Id
idFromBody value =
  { projectId: _, revisionNumber: _ }
    <$> Json.decodeAtField "projectId" value projectIdFromBody
    <*> Json.decodeAtField "revisionNumber" value Json.decodeInt
    <#> Id
  where
    decodeUuid ∷ Json → Either Json.Error Uuid
    decodeUuid value =
      Json.decodeString value >>= \string →
        case Uuid.base49Decode string of
          Just uuid → Right uuid
          Nothing → Left (Json.Failure "Expecting a UUID" value)


-- REVISION


newtype Revision =
  Revision
    { htmlCode ∷ String
    , elmCode ∷ String
    , packages ∷ Array Package
    , title ∷ String
    , elmVersion ∷ Version
    , acceptedTerms ∷ Maybe TermsVersion
    , userId ∷ Maybe User.Id
    }

derive instance newtypeRevision ∷ Newtype Revision _


fromPostgres ∷ Json → Either Json.Error Revision
fromPostgres value =
  { htmlCode: _, elmCode: _, packages: _, title: _,  elmVersion: _, acceptedTerms: _, userId: _ }
    <$> Json.decodeAtField "html_code" value Json.decodeString
    <*> Json.decodeAtField "elm_code" value Json.decodeString
    <*> Json.decodeAtField "packages" value (Json.decodeArray Package.fromPostgres)
    <*> Json.decodeAtField "title" value Json.decodeString
    <*> Json.decodeAtField "elm_version" value Version.fromPostgres
    <*> Json.decodeAtField "terms_version" value (Json.decodeNullable TermsVersion.fromJson)
    <*> Json.decodeAtField "user_id" value (Json.decodeNullable User.idFromPostgresValue)
    <#> Revision


toPostgres ∷ Revision → Array Json.KeyValue
toPostgres (Revision r) =
    [ { key: "html_code", value: Json.encodeString r.htmlCode }
    , { key: "elm_code", value: Json.encodeString r.elmCode }
    , { key: "packages", value: Json.encodeArray Package.toPostgres r.packages }
    , { key: "title", value: Json.encodeString r.title }
    , { key: "elm_version", value: Version.toPostgres r.elmVersion }
    , { key: "terms_version", value: Json.encodeNullable TermsVersion.toJson r.acceptedTerms }
    , { key: "user_id", value: Json.encodeNullable User.idToPostgresValue r.userId }
    ]


toBody ∷ Revision → Json
toBody (Revision r) =
  Json.encodeObject
    [ { key: "htmlCode", value: Json.encodeString r.htmlCode }
    , { key: "elmCode", value: Json.encodeString r.elmCode }
    , { key: "packages", value: Json.encodeArray Package.toBody r.packages }
    , { key: "title", value: Json.encodeString r.title }
    , { key: "elmVersion", value: Version.toBody r.elmVersion }
    , { key: "termsVersion", value: Json.encodeNullable TermsVersion.toJson r.acceptedTerms }
    , { key: "userId", value: Json.encodeNullable User.idToBody r.userId }
    ]


fromBody ∷ Json → Either Json.Error Revision
fromBody value =
  { htmlCode: _, elmCode: _, packages: _, title: _,  elmVersion: _, acceptedTerms: _, userId: _ }
    <$> Json.decodeAtField "htmlCode" value Json.decodeString
    <*> Json.decodeAtField "elmCode" value Json.decodeString
    <*> Json.decodeAtField "packages" value (Json.decodeArray Package.fromBody)
    <*> Json.decodeAtField "title" value Json.decodeString
    <*> Json.decodeAtField "elmVersion" value Version.fromBody
    <*> Json.decodeAtField "termsVersion" value (Json.decodeNullable TermsVersion.fromJson)
    <*> Json.decodeAtField "userId" value (Json.decodeNullable User.idFromBody)
    <#> Revision


-- ENTITY


instance identifiedByIdRevision ∷ IdentifiedBy Id Revision


entityToPostgres ∷ Entity Id Revision → Array Json.KeyValue
entityToPostgres entity =
  idToPostgres (Entity.key entity) <> toPostgres (Entity.record entity)


entityFromPostgres ∷ Json → Either Json.Error (Entity Id Revision)
entityFromPostgres value =
  Entity.entity
    <$> idFromPostgres value
    <*> fromPostgres value


entityToBody ∷ Entity Id Revision → Json
entityToBody entity =
  Json.encodeObject
    [ { key: "key", value: idToBody $ Entity.key entity }
    , { key: "record", value: toBody $ Entity.record entity }
    ]


entityFromBody ∷ Json → Either Json.Error (Entity Id Revision)
entityFromBody value =
  Entity.entity
    <$> Json.decodeAtField "key" value idFromBody
    <*> Json.decodeAtField "record" value fromBody
