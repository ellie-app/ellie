module TheMasterPlan.CanonicalModule where

import Ellie.Prelude

import Data.Foreign as Foreign
import Data.Foreign.Class (class Foreignable, get, put)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Elm.Compiler.Module.Name.Canonical (Canonical(..))
import Elm.Compiler.Module.Name.Raw (Raw(..))
import Elm.Compiler.Module.Name.Raw as Raw
import Elm.Compiler.Version as Compiler
import Elm.Package (Package(..))
import Elm.Package.Name (Name(..))
import System.FileSystem (FilePath, (</>), (<.>))


newtype CanonicalModule =
  CanonicalModule
    { package :: Package
    , name :: Raw
    }


derive instance eqCanonicalModule :: Eq CanonicalModule
derive instance ordCanonicalModule :: Ord CanonicalModule
derive instance newtypeCanonicalModule :: Newtype CanonicalModule _


instance foreignableCanonicalModule :: Foreignable CanonicalModule where
  put (CanonicalModule { package, name }) =
    Foreign.toForeign
      { package: put package
      , name: put name
      }
  
  get input =
    { package: _, name: _ }
      <$> (input ! "package" >>= get)
      <*> (input ! "name" >>= get)
      <#> CanonicalModule


interfacePath :: CanonicalModule -> FilePath
interfacePath (CanonicalModule { package: (Package p), name }) =
  "/elm-stuff/build-artifacts"
    </> show Compiler.version
    </> show p.name
    </> show p.version
    </> Raw.hyphenate name
    <.> "elmi"


objectPath :: CanonicalModule -> FilePath
objectPath (CanonicalModule { package: (Package p), name }) =
  "/elm-stuff/build-artifacts"
    </> show Compiler.version
    </> show p.name
    </> show p.version
    </> Raw.hyphenate name
    <.> "elmo"


simplify :: CanonicalModule -> Canonical
simplify (CanonicalModule { package: (Package { name: package }), name }) =
  Canonical { package: package, modul: name }


isElmLang :: CanonicalModule -> Boolean
isElmLang (CanonicalModule { package: (Package { name: (Name n)}) }) =
  n.user == "elm-lang"


toVarString :: CanonicalModule -> String
toVarString (CanonicalModule { package: Package { name: Name { user, project }, version }, name: Raw name }) =
  let
    safeUser =
      String.replaceAll (Pattern "-") (Replacement "_") user

    safeProject =
        String.replaceAll (Pattern "-") (Replacement "_") project

    safeModuleName =
        String.joinWith "_" name
  in
    "_" <> safeUser <> "$" <> safeProject <> "$" <> safeModuleName


qualifiedVar :: CanonicalModule -> String -> String
qualifiedVar modul var =
    toVarString modul <> "$" <> String.replaceAll (Pattern "'") (Replacement "$") var
