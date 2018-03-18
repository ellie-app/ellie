module Elm.Project
  ( Project(..)
  , default
  , toFile
  , fromFile
  )
  where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Json (Json)
import Data.Json as Json
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Elm.Package (Package(..))
import Elm.Name as Name
import Elm.Version (Version(..))
import Elm.Version as Version


newtype Project =
  Project
    { sourceDirs ∷ Array String
    , elmVersion ∷ Version
    , deps ∷ Set Package
    , transDeps ∷ Set Package
    }

derive instance newtypeDescription ∷ Newtype Project _


default ∷ Project
default =
  Project
    { sourceDirs: [ "src" ]
    , elmVersion: Version { major: 0, minor: 19, patch: 0 }
    , deps: Set.empty
    , transDeps: Set.empty
    }


-- SERIALIZATIONS


toFile ∷ Project → String
toFile (Project value) =
  Json.stringify $ Json.encodeObject
    [ { key: "type", value: Json.encodeString "browser" }
    , { key: "source-directories", value: Json.encodeArray Json.encodeString value.sourceDirs }
    , { key: "elm-version", value: Json.encodeString $ Version.toString value.elmVersion }
    , { key: "dependencies", value: encodeDependencies value.deps }
    , { key: "test-dependencies", value: Json.encodeObject [] }
    , { key: "do-not-edit-this-by-hand"
      , value: Json.encodeObject [ { key: "transitive-dependencies", value: encodeDependencies value.transDeps } ]
      }
    ]
  where
    encodeDependencies ∷ Set Package → Json
    encodeDependencies values =
      values
        # Array.fromFoldable
        # map (\(Package { name, version }) → { key: Name.toString name, value: Json.encodeString $ Version.toString version })
        # Json.encodeObject


fromFile ∷ String → Either String Project
fromFile string =
  lmap Json.errorToString $ Json.parse string >>= \value →
    { sourceDirs: _, elmVersion: _,  deps: _, transDeps: _ }
      <$> Json.decodeAtField "source-directories" value (Json.decodeArray Json.decodeString)
      <*> Json.decodeAtField "elm-version" value (\v → Json.decodeString v >>= (Version.fromString >>> Either.note (Json.Failure "Expecting a version MAJOR.MINOR.PATCH" v)))
      <*> Json.decodeAtField "dependencies" value decodeDependencies
      <*> Json.decodeAtField "do-not-edit-this-by-hand" value (\v → Json.decodeAtField "transitive-dependencies" v decodeDependencies)
      <#> Project
  where
    decodeDependencies ∷ Json → Either Json.Error (Set Package)
    decodeDependencies object =
      Json.decodeKeyValues Right object
        >>= traverse 
            (\{ key, value } →
                { name: _, version: _ }
                  <$> Either.note
                          (Json.Failure "Expecting a name USER/PROJECT" (Json.encodeString key))
                          (Name.fromString key)
                  <*> (Json.decodeString value >>= \string →
                        Either.note
                          (Json.Failure "Expecting a version MAJOR.MINOR.PATCH" (Json.encodeString key))
                          (Version.fromString string)
                      )
                  <#> Package
            )
        <#> Set.fromFoldable
