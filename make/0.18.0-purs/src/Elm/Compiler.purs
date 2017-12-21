module Elm.Compiler
    ( parseDependencies
    , compile
    , makeCompiler
    , COMPILER
    , Compiler
    , Result
    )
    where

import Ellie.Prelude

import Control.Monad.Except (except, runExcept)
import Control.Monad.Task (kind Effect, Task)
import Control.Monad.Task as Task
import Data.Array (intercalate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foreign (Foreign, F)
import Data.Foreign (ForeignError(JSONError), isArray, readArray, readString, renderForeignError, toForeign) as Foreign
import Data.Foreign.Class (get, put)
import Data.Foreign.Index ((!))
import Data.Foreign.Index (hasOwnProperty) as Foreign
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn2, runFn4)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Url (Url)
import Elm.Compiler.Error (Error)
import Elm.Compiler.Module.Interface (Interface)
import Elm.Compiler.Module.Name.Raw (Raw(..))
import Elm.Package (Package(..))
import Elm.Package.Name (Name(..))
import TheMasterPlan.CanonicalModule (CanonicalModule(..))

foreign import data COMPILER :: Effect
foreign import data Compiler :: Type
foreign import _compile :: ∀ e. Fn2 Compiler (Array Foreign) (Task.EffFnTask (compiler :: COMPILER | e) String Foreign)
foreign import _parseDependencies :: ∀ e. Fn2 Compiler (Array Foreign) (Task.EffFnTask (compiler :: COMPILER | e) String Foreign)
foreign import _parseJson :: Fn2 Task.FfiHelpers String (Either String Foreign)
foreign import _makeCompiler :: ∀ e. String -> Task.EffFnTask (compiler :: COMPILER | e) String Compiler


parseJson :: String -> F Foreign
parseJson input =
  input
    |> runFn2 _parseJson Task.ffiHelpers 
    |> lmap (Foreign.JSONError >>> NonEmptyList.singleton)
    |> except


recoverErrors :: ∀ e. Foreign -> F (Array Error)
recoverErrors input = do
  array <- Foreign.readArray input
  arrayOfStrings <- traverse Foreign.readString array
  parsed <- traverse parseJson arrayOfStrings
  traverse get parsed


recoverDependencies :: ∀ e. Foreign -> F (Either (Array Error) { name :: Raw, dependencies :: Array Raw })
recoverDependencies input = do
  firstItem <- input ! 0
  case unit of
    _ | not (Foreign.isArray firstItem) ->
        Left <$> recoverErrors input
      | not (Foreign.hasOwnProperty 1 input) ->
        Left <$> recoverErrors firstItem
      | otherwise ->
        { name: _, dependencies: _ }
          <$> (get firstItem <#> Raw)
          <*> (input ! 1 >>= get <#> map Raw)
          <#> Right


makeParseArgs :: Name -> String -> Array Foreign
makeParseArgs (Name { user, project }) source =
  [ Foreign.toForeign [ user, project ]
  , Foreign.toForeign source
  ]


parseDependencies ::
  ∀  e
  .  Compiler
  -> Name
  -> String
  -> Task
      (compiler :: COMPILER | e)
      String
      (Either (Array Error) { name :: Raw, dependencies :: Array Raw })
parseDependencies compiler name source = do
  output <-
    runFn2 _parseDependencies compiler (makeParseArgs name source)
      |> Task.fromEffFnTask "Compiler.parseDependencies"

  recoverDependencies output
    |> runExcept
    |> lmap (map Foreign.renderForeignError >>> intercalate "\n" )
    |> Task.fromEither


type Result =
    { interface :: Interface
    , js :: String
    }


makeCompileArgs :: Name -> String -> Map CanonicalModule Interface -> Array Foreign
makeCompileArgs (Name { user, project }) source interfaces =
  [ put [ user, project ]
  , put false
  , put source
  , interfaces
      |> (Map.toUnfoldable :: ∀ k v. Map k v -> Array (Tuple k v))
      |> map
          (\(Tuple (CanonicalModule { package: Package { name: Name n, version }, name }) interface) ->
            [ put
                [ put
                    [ put [ n.user, n.project ]
                    , put (unwrap name)
                    ]
                 , put version
                 ]
            , put interface
            ]
          )
      |> put
  ]


recoverResult :: Foreign -> F (Either (Array Error) Result)
recoverResult input = do
  success <- input ! 0 >>= Foreign.readString <#> (_ == "true")
  if success
    then
      { interface: _, js: _ }
        <$> (input ! 1 >>= get)
        <*> (input ! 2 >>= get)
        <#> Right
    else
      input ! 1
        >>= recoverErrors
        <#> Left


compile ::
  ∀  e
  .  Compiler
  -> Name
  -> String
  -> Map CanonicalModule Interface
  -> Task (compiler :: COMPILER | e) String (Either (Array Error) Result)
compile compiler name source interfaces = do
  output <-
    runFn2 _compile compiler (makeCompileArgs name source interfaces)
      |> Task.fromEffFnTask "Compiler.compile"

  recoverResult output
    |> runExcept
    |> lmap (map Foreign.renderForeignError >>> intercalate "\n")
    |> Task.fromEither


makeCompiler :: ∀ e. Url -> Task (compiler :: COMPILER | e) String Compiler
makeCompiler =
  show >>> _makeCompiler >>> Task.fromEffFnTask "Compiler.makeCompiler"