module Ellie.Elm.Package.Description
  ( Description(..)
  )
  where

import Prelude
import Data.FilePath (FilePath)
import Data.Foreign (Foreign, F)
import Data.Foreign (ForeignError(ForeignError), fail, toForeign, unsafeReadTagged) as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Ellie.Elm.Package (Package(..))
import Ellie.Elm.Package.Name (Name)
import Ellie.Elm.Package.Constraint (Constraint)
import Ellie.Elm.Package.Constraint as Constraint
import Ellie.Elm.Package.Version (Version)


newtype Description =
  Description
    { elmVersion :: Constraint
    , sourceDirs :: Array FilePath
    , dependencies :: Array Package
    }

derive instance newtypeDescription :: Newtype Description _

instance encodeDescription :: Encode Description where
  encode (Description value) =
    Foreign.toForeign
      { "elm-version": Foreign.encode value.elmVersion
      , "source-directories": Foreign.encode value.sourceDirs
      , dependencies: putDependencies value.dependencies
      }

instance decodeDescription :: Decode Description where
  decode value =
    { elmVersion: _, sourceDirs: _, dependencies: _ }
      <$> (value ! "elm-version" >>= Foreign.decode)
      <*> (value ! "source-directories" >>= Foreign.decode)
      <*> (value ! "dependencies" >>= getDependencies)
      <#> Description


getName :: Foreign -> F Name
getName value = do
  string <- Foreign.decode value
  case String.split (Pattern "github.com/") string of
    [ _, name ] ->
      name
        # String.replace (Pattern ".git") (Replacement "")
        # Foreign.toForeign
        # Foreign.decode

    _ ->
      Foreign.fail (Foreign.ForeignError "Bad repo")


getDependencies :: Foreign -> F (Array Package)
getDependencies object =
  object
    # Foreign.unsafeReadTagged "Object"
    <#> StrMap.toUnfoldable
    >>= traverse 
        (\(Tuple k v) ->
            { name: _, version: _ }
              <$> Foreign.decode (Foreign.toForeign k)
              <*> (Foreign.decode v <#> Constraint.lowestVersion)
              <#> Package
        )


putDependencies :: Array Package -> Foreign
putDependencies values =
  values
    # map (\(Package { name, version }) -> Tuple (show name) (Foreign.encode version))
    # StrMap.fromFoldable
    # Foreign.toForeign


