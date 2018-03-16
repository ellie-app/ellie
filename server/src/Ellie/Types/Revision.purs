module Ellie.Types.Revision
  ( Revision(..)
  , Id(..)
  ) where

import Prelude

import Data.Entity (class IdentifiedBy)
import Data.Json (decodeAtField, encodeObject) as Json
import Data.Maybe (Maybe)
import Data.String.Class (class ToString)
import Data.String.Class (toString) as String
import Data.Uuid (Uuid)
import Data.Uuid as Uuid
import Ellie.Types.TermsVersion (TermsVersion)
import Elm.Package (Package)
import Elm.Version (Version)
import Server.Action (class IsBody)
import Server.Action as Action
import System.Postgres (class FromResult, class ToArguments)
import System.Postgres as Postgres

instance identifiedByIdRevision ∷ IdentifiedBy Id Revision

newtype Id =
  Id { projectId ∷ Uuid, revisionNumber ∷ Int }

instance toStringId ∷ ToString Id where
  toString (Id { projectId, revisionNumber }) =
    Uuid.encode projectId <> "/" <> String.toString revisionNumber

instance fromResultId ∷ FromResult Id where
  fromResult value =
    { projectId: _, revisionNumber: _ }
      <$> Json.decodeAtField "project_id" value Postgres.fromResult
      <*> Json.decodeAtField "revision_number" value Postgres.fromResult
      <#> Id

instance toArgumentsId ∷ ToArguments Id where
  toArguments (Id { projectId, revisionNumber }) =
      [ { key: "project_id", value: Postgres.toValue projectId }
      , { key: "revision_number", value: Postgres.toValue revisionNumber }
      ]

instance isBodyId ∷ IsBody Id where
  toBody (Id {projectId, revisionNumber }) =
    Json.encodeObject
      [ { key: "projectId", value: Action.toBody projectId }
      , { key: "revisionNumber", value: Action.toBody revisionNumber }
      ]
  fromBody value =
    { projectId: _, revisionNumber: _ }
      <$> Json.decodeAtField "projectId" value Action.fromBody
      <*> Json.decodeAtField "revisionNumber" value Action.fromBody
      <#> Id



newtype Revision =
  Revision
    { htmlCode ∷ String
    , elmCode ∷ String
    , packages ∷ Array Package
    , title ∷ String
    , elmVersion ∷ Version
    , acceptedTerms ∷ Maybe TermsVersion
    }

instance fromResultRevision ∷ FromResult Revision where
  fromResult value =
    { htmlCode: _, elmCode: _, packages: _, title: _,  elmVersion: _, acceptedTerms: _ }
      <$> Json.decodeAtField "html_code" value Postgres.fromResult
      <*> Json.decodeAtField "elm_code" value Postgres.fromResult
      <*> Json.decodeAtField "packages" value Postgres.fromResult
      <*> Json.decodeAtField "title" value Postgres.fromResult
      <*> Json.decodeAtField "elm_version" value Postgres.fromResult
      <*> Json.decodeAtField "terms_version" value Postgres.fromResult
      <#> Revision

instance toArgumentsRevision ∷ ToArguments Revision where
  toArguments (Revision r) =
      [ { key: "html_code", value: Postgres.toValue r.htmlCode }
      , { key: "elm_code", value: Postgres.toValue r.elmCode }
      , { key: "packages", value: Postgres.toValue r.packages }
      , { key: "title", value: Postgres.toValue r.title }
      , { key: "elm_version", value: Postgres.toValue r.elmVersion }
      , { key: "terms_version", value: Postgres.toValue r.acceptedTerms }
      ]

instance isBodyRevision ∷ IsBody Revision where
  toBody (Revision r) =
    Json.encodeObject
      [ { key: "htmlCode", value: Action.toBody r.htmlCode }
      , { key: "elmCode", value: Action.toBody r.elmCode }
      , { key: "packages", value: Action.toBody r.packages }
      , { key: "title", value: Action.toBody r.title }
      , { key: "elmVersion", value: Action.toBody r.elmVersion }
      , { key: "termsVersion", value: Action.toBody r.acceptedTerms }
      ]
  fromBody value =
    { htmlCode: _, elmCode: _, packages: _, title: _,  elmVersion: _, acceptedTerms: _ }
      <$> Json.decodeAtField "htmlCode" value Action.fromBody
      <*> Json.decodeAtField "elmCode" value Action.fromBody
      <*> Json.decodeAtField "packages" value Action.fromBody
      <*> Json.decodeAtField "title" value Action.fromBody
      <*> Json.decodeAtField "elmVersion" value Action.fromBody
      <*> Json.decodeAtField "termsVersion" value Action.fromBody
      <#> Revision
