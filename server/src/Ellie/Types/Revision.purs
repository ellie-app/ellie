module Ellie.Types.Revision
  ( Revision(..)
  , Id(..)
  , Snapshot(..)
  ) where

import Prelude

import Data.Entity (class IdentifiedBy)
import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Foreign.Index ((!))
import Data.Foreign.NullOrUndefined (readNullOrUndefined, unNullOrUndefined) as Foreign
import Data.Maybe (Maybe)
import Elm.Compiler.Error (Error)
import Elm.Package (Package)
import Elm.Package.Version (Version)
import Ellie.Types.TermsVersion (TermsVersion)
import Ellie.Types.ProjectId (ProjectId)

instance identifiedByIdRevision ∷ IdentifiedBy Id Revision

newtype Id =
  Id { projectId ∷ ProjectId, revisionNumber :: Int }

instance showId ∷ Show Id where
  show (Id { projectId, revisionNumber }) =
    show projectId <> "/" <> show revisionNumber

instance decodeId ∷ Decode Id where
  decode object =
    { projectId: _, revisionNumber: _ }
      <$> (object ! "projectId" >>= Foreign.decode)
      <*> (object ! "revisionNumber" >>= Foreign.decode)
      <#> Id

instance encodeId ∷ Encode Id where
  encode (Id { projectId, revisionNumber }) =
    Foreign.toForeign
      { projectId: Foreign.encode projectId
      , revisionNumber
      }


newtype Revision =
  Revision
    { htmlCode ∷ String
    , elmCode :: String
    , packages ∷ Array Package
    , title :: String
    , description ∷ String
    , snapshot ∷ Snapshot
    , elmVersion :: Version
    , acceptedTerms ∷ Maybe TermsVersion
    }


instance decodeRevision ∷ Decode Revision where
  decode object =
    { htmlCode: _, elmCode: _, packages: _, title: _, description: _, snapshot: _, elmVersion: _, acceptedTerms: _ }
      <$> (object ! "htmlCode" >>= Foreign.decode)
      <*> (object ! "elmCode" >>= Foreign.decode)
      <*> (object ! "packages" >>= Foreign.decode)
      <*> (object ! "title" >>= Foreign.decode)
      <*> (object ! "description" >>= Foreign.decode)
      <*> (object ! "snapshot" >>= Foreign.decode)
      <*> (object ! "elmVersion" >>= Foreign.decode)
      <*> (object ! "acceptedTerms" >>= Foreign.readNullOrUndefined Foreign.decode <#> Foreign.unNullOrUndefined)
      <#> Revision


instance encodeRevision ∷ Encode Revision where
  encode (Revision { }) =
    Foreign.toForeign {}


data Snapshot
  = NotSaved
  | Uploaded
  | Errored (Array Error)


instance decodeSnapshot ∷ Decode Snapshot where
  decode object =
    object ! "result" >>= Foreign.decode >>= \result →
      case result of
        "ERROR" -> (object ! "errors" >>= Foreign.decode <#> Errored)
        "SUCCESS" → pure Uploaded
        _ → pure NotSaved
