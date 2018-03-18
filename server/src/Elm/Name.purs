module Elm.Name
  ( Name(..)
  , core
  , html
  , browser
  , toString
  , fromString
  , toBody
  , fromBody
  , toPostgres
  , fromPostgres
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Json (Json)
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String (split) as String


newtype Name =
  Name
    { user ∷ String
    , project ∷ String
    }

derive instance eqName ∷ Eq Name
derive instance ordName ∷ Ord Name
derive instance newtypeName ∷ Newtype Name _


core ∷ Name
core =
  Name { user: "elm-lang", project: "core" }


html ∷ Name
html =
  Name { user: "elm-lang", project: "html" }


browser ∷ Name
browser =
  Name { user: "elm-lang", project: "browser" }


-- SERIALIZATIONS


toString ∷ Name → String
toString (Name { user, project }) =
  user <> "/" <> project


fromString ∷ String → Maybe Name
fromString string =
  case String.split (Pattern "/") string of
    [user, project] → Just $ Name { user, project }
    _ → Nothing


toBody ∷ Name → Json
toBody name =
  Json.encodeString $ toString name


fromBody ∷ Json → Either Json.Error Name
fromBody value =
  Json.decodeString value >>= \string →
    case fromString string of
      Just name → Right name
      Nothing → Left $ Json.Failure "Package name must be a string in the format user/project" value


fromPostgres ∷ Json → Either Json.Error Name
fromPostgres value =
  { user: _, project: _ }
    <$> Json.decodeAtField "username" value Json.decodeString
    <*> Json.decodeAtField "project" value Json.decodeString
    <#> Name


toPostgres ∷ Name → Json
toPostgres (Name { user, project }) =
  Json.encodeObject
    [ { key: "username", value: Json.encodeString user }
    , { key: "project", value: Json.encodeString project }
    ]
