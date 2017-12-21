module Elm.Package.Name where

import Prelude
import Data.Read (class Read)
import Data.Either (Either(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Foreign (toForeign, ForeignError(ForeignError), fail) as Foreign
import Data.Foreign.Class (class Foreignable)
import Data.Foreign.Class (get) as Foreign

newtype Name =
  Name
    { user :: String
    , project :: String
    }

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name


instance foreignableName :: Foreignable Name where
  put = show >>> Foreign.toForeign
  
  get value = do
    string <- Foreign.get value
    case String.split (Pattern "/") string of
      [user, project] -> pure (Name { user, project })
      _ -> Foreign.fail (Foreign.ForeignError "Expected a package name in the format `user/project`")

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
