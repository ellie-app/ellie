module Ellie.Types.User
  ( User(..)
  , Id(..)
  , default
  ) where

import Prelude

import Data.Entity (class IdentifiedBy)
import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Foreign.Index ((!))
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Foreign.NullOrUndefined (unNullOrUndefined) as Foreign
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.Class (class ToString)
import Data.UniqueId (UniqueId(..))
import Ellie.Types.ProjectId (ProjectId)


instance identifiedByIdUser ∷ IdentifiedBy Id User


newtype Id
  = Id UniqueId

derive instance newtypeId ∷ Newtype Id _
derive newtype instance toStringId ∷ ToString Id
derive newtype instance encodeId ∷ Encode Id


newtype User =
  User
    { latestTermsVersion :: Maybe Int
    , ownedProjects :: Array ProjectId
    }

derive instance newtypeUser :: Newtype User _

instance encodeUser :: Encode User where
  encode (User u) =
    Foreign.toForeign
      { latestTermsVersion: Foreign.encode (NullOrUndefined u.latestTermsVersion)
      , ownedProjects: Foreign.encode u.ownedProjects
      }

instance decodeSession :: Decode User where
  decode json =
    { latestTermsVersion: _, ownedProjects: _ }
      <$> (json ! "latestTermsVersion" >>= Foreign.decode <#> Foreign.unNullOrUndefined)
      <*> (json ! "ownedProjects" >>= Foreign.decode)
      <#> User


default :: User
default =
  User
    { latestTermsVersion: Nothing
    , ownedProjects: []
    }