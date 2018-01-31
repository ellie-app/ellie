module Ellie.Elm.Package.Name where

import Prelude

import Data.Foreign as Foreign
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Class as Foreign
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as String

newtype Name =
  Name
    { user :: String
    , project :: String
    }

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name
derive instance newtypeName :: Newtype Name _

instance showName :: Show Name where
  show (Name { user, project }) =
    user <> "/" <> project

instance decodeName :: Decode Name where
  decode value = do
    string <- Foreign.decode value
    case String.split (Pattern "/") string of
      [user, project] ->
        pure $ Name { user, project }
      _ ->
        Foreign.fail $ Foreign.ForeignError "Package name must be in the format user/project"

instance encodeName :: Encode Name where
  encode = show >>> Foreign.encode