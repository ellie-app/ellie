module Elm.Package.Name where

import Prelude
import Data.Read (class Read)
import Data.Either (Either(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Foreign (toForeign) as Foreign
import Data.Foreign.Class (class Foreignable)
import Data.Foreign.Class (get, put) as Foreign
import Data.Foreign.Index ((!))

newtype Name =
  Name
    { user :: String
    , project :: String
    }

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name


instance foreignableName :: Foreignable Name where
  put (Name { user, project }) =
    Foreign.toForeign { user: Foreign.put user, project: Foreign.put project }

  get value =
    { user: _, project: _ }
      <$> (value ! "user" >>= Foreign.get)
      <*> (value ! "project" >>= Foreign.get)
      <#> Name

instance readName :: Read Name where
  read input =
    case String.split (Pattern "/") input of
      [ user, project ] ->
        Right $ Name { user, project }

      _ ->
        Left "Expecting a project name with format `user/project`"

instance showName :: Show Name where
  show (Name { user, project }) =
    user <> "/" <> project


core :: Name
core =
  Name { user: "elm-lang", project: "core" }

virtualDom :: Name
virtualDom =
  Name { user: "elm-lang", project: "virtual-dom" }

html :: Name
html =
  Name { user: "elm-lang", project: "html" }
