module Elm.Name
  ( Name(..)
  , core
  , html
  , browser
  , isCore
  , isHtml
  , isBrowser
  -- eqName
  -- ordName
  -- newtypeName
  -- showName
  -- toStringName
  -- fromJsonName
  -- toJsonName
  -- toFieldName
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Class (class FromString, class ToString)
import Data.String.Class as String
import Server.Action (class IsBody)
import Server.Action as Action
import System.Postgres (class ToValue, class FromResult)
import System.Postgres as Postgres


core ∷ Name
core =
  Name { user: "elm-lang", project: "core" }


html ∷ Name
html =
  Name { user: "elm-lang", project: "html" }


browser ∷ Name
browser =
  Name { user: "elm-lang", project: "browser" }


isCore ∷ Name → Boolean
isCore (Name { user, project }) =
  user == "elm-lang" && project == "core"


isHtml ∷ Name → Boolean
isHtml (Name { user, project }) =
  user == "elm-lang" && project == "html"


isBrowser ∷ Name → Boolean
isBrowser name =
  name == browser


newtype Name =
  Name
    { user ∷ String
    , project ∷ String
    }

derive instance eqName ∷ Eq Name
derive instance ordName ∷ Ord Name
derive instance newtypeName ∷ Newtype Name _

instance showName ∷ Show Name where
  show (Name { user, project }) =
    "Name { user: \"" <> user <> "\", project: \"" <> project <> "\" }"

instance toStringName ∷ ToString Name where
  toString (Name { user, project }) =
    user <> "/" <> project

instance fromStringName ∷ FromString Name where
  fromString string =
    case String.split (Pattern "/") string of
      [user, project] → Just $ Name { user, project }
      _ → Nothing

instance isBodyName ∷ IsBody Name where
  fromBody value =
    Json.decodeString value >>= \string →
      case String.fromString string of
        Just name → Right name
        Nothing → Left $ Json.Failure "Package name must be a string in the format user/project" value

  toBody name =
    Json.encodeString $ String.toString name

instance fromResultName ∷ FromResult Name where
  fromResult value =
    { user: _, project: _ }
      <$> Json.decodeAtField "username" value Postgres.fromResult
      <*> Json.decodeAtField "project" value Postgres.fromResult
      <#> Name

instance toValueName ∷ ToValue Name where
  toValue (Name { user, project }) =
    Json.encodeObject
      [ { key: "username", value: Postgres.toValue user }
      , { key: "project", value: Postgres.toValue project }
      ]
