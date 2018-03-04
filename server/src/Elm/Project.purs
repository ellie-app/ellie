module Elm.Project
  ( Project(..)
  , default
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
import Data.String.Class (toString) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Elm.Package (Package(..))
import Elm.Package.Constraint (Constraint)
import Elm.Package.Constraint as Constraint
import Elm.Package.Name (Name)
import Elm.Package.Version (Version(..))


newtype Project =
  Project
    { sourceDirs ∷ Array String
    , elmVersion ∷ Version
    , deps ∷ Set Package
    , transDeps ∷ Set Package
    }

derive instance newtypeDescription ∷ Newtype Project _

instance encodeProject ∷ Encode Project where
  encode (Project value) =
    Foreign.toForeign
      { "type": "browser"
      , "source-directories": Foreign.encode value.sourceDirs
      , "elm-version": Foreign.encode value.elmVersion
      , "dependencies": putDependencies value.deps
      , "test-dependencies": {}
      , "do-not-edit-this-by-hand": {
          "transitive-dependencies": putDependencies value.transDeps
        }
      }
    where
      putDependencies ∷ Set Package → Foreign
      putDependencies values =
        values
          # Array.fromFoldable
          # map (\(Package { name, version }) → Tuple (String.toString name) (Foreign.encode version))
          # StrMap.fromFoldable
          # Foreign.toForeign

instance decodeProject ∷ Decode Project where
  decode value =
    {  sourceDirs: _, elmVersion: _,  deps: _, transDeps: _ }
      <$> (value ! "source-directories" >>= Foreign.decode)
      <*> (value ! "elm-version" >>= Foreign.decode)
      <*> (value ! "dependencies" >>= getDependencies)
      <*> (value ! "do-not-edit-this-by-hand" >>= (_ ! "transitive-dependencies") >>= getDependencies)
      <#> Project
    where
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


default ∷ Project
default =
  Project
    { sourceDirs: [ "src" ]
    , elmVersion: Version { major: 0, minor: 19, patch: 0 }
    , deps: Set.empty
    , transDeps: Set.empty
    }