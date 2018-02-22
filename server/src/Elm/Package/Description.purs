module Elm.Package.Description
  ( Description(..)
  )
  where

import Prelude

import Data.Array as Array
import Data.FilePath (FilePath)
import Data.Foreign (Foreign, F)
import Data.Foreign (ForeignError(ForeignError), fail, toForeign, unsafeReadTagged) as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Class (decode, encode) as Foreign
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Elm.Package (Package(..))
import Elm.Package.Constraint (Constraint)
import Elm.Package.Constraint as Constraint
import Elm.Package.Name (Name)
import Elm.Package.Version (Version)


newtype Description =
  Description
    { repo ∷ String
    , version ∷ Version
    , summary ∷ String
    , license ∷ String
    , sourceDirs ∷ Array FilePath
    , exposed ∷ Array String
    , elmVersion ∷ Constraint
    , dependencies ∷ Set Package
    }

derive instance newtypeDescription ∷ Newtype Description _

instance encodeDescription ∷ Encode Description where
  encode (Description value) =
    Foreign.toForeign
      { repository: value.repo
      , version: Foreign.encode value.version
      , summary: value.summary
      , license: value.license
      , "source-directories": Foreign.encode value.sourceDirs
      , "exposed-modules": value.exposed
      , "elm-version": Foreign.encode value.elmVersion
      , dependencies: putDependencies value.dependencies
      }

instance decodeDescription ∷ Decode Description where
  decode value =
    { repo: _, version: _, summary: _, license: _
    , sourceDirs: _, exposed: _, elmVersion: _,  dependencies: _
    }
      <$> (value ! "repository" >>= Foreign.decode)
      <*> (value ! "version" >>= Foreign.decode)
      <*> (value ! "summary" >>= Foreign.decode)
      <*> (value ! "license" >>= Foreign.decode)
      <*> (value ! "source-directories" >>= Foreign.decode)
      <*> (value ! "exposed-modules" >>= Foreign.decode)
      <*> (value ! "elm-version" >>= Foreign.decode)
      <*> (value ! "dependencies" >>= getDependencies)
      <#> Description


getName ∷ Foreign → F Name
getName value = do
  string ← Foreign.decode value
  case String.split (Pattern "github.com/") string of
    [ _, name ] →
      name
        # String.replace (Pattern ".git") (Replacement "")
        # Foreign.toForeign
        # Foreign.decode

    _ →
      Foreign.fail (Foreign.ForeignError "Bad repo")


getDependencies ∷ Foreign → F (Set Package)
getDependencies object =
  object
    # Foreign.unsafeReadTagged "Object"
    <#> (StrMap.toUnfoldable ∷ StrMap Foreign → Array (Tuple String Foreign))
    >>= traverse 
        (\(Tuple k v) →
            { name: _, version: _ }
              <$> Foreign.decode (Foreign.toForeign k)
              <*> (Foreign.decode v <#> Constraint.lowestVersion)
              <#> Package
        )
    <#> Set.fromFoldable


putDependencies ∷ Set Package → Foreign
putDependencies values =
  values
    # Array.fromFoldable
    # map (\(Package { name, version }) → Tuple (show name) (Foreign.encode (Constraint.forVersion version)))
    # StrMap.fromFoldable
    # Foreign.toForeign
