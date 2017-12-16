module Elm.Compiler
    ( parseDependencies
    , compile
    , COMPILER
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
import Data.Function.Uncurried (Fn2, Fn4, runFn2, runFn4)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Url (Url)
import Elm.Compiler.Error (Error)
import Elm.Compiler.Module.Interface (Interface)
import Elm.Compiler.Module.Name.Raw (Raw)
import Elm.Package (Package(..))
import Elm.Package.Name (Name(..))
import TheMasterPlan.CanonicalModule (CanonicalModule(..))


foreign import data COMPILER :: Effect

foreign import _exec :: ∀ e. Fn4 Task.FfiHelpers Url String (Array Foreign) (Task.EffFnTask (compiler :: COMPILER | e) String Foreign)

foreign import _parseJson :: Fn2 Task.FfiHelpers String (Either String Foreign)


parseJson :: String -> F Foreign
parseJson input =
  input
    |> runFn2 _parseJson Task.ffiHelpers 
    |> lmap (Foreign.JSONError >>> NonEmptyList.singleton)
    |> except


exec :: ∀ e. Url -> String -> Array Foreign -> Task (compiler :: COMPILER | e) String Foreign
exec compilerUrl channel args =
  Task.fromEffFnTask <| runFn4 _exec Task.ffiHelpers compilerUrl channel args


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
          <$> (get firstItem)
          <*> (input ! 1 >>= get)
          <#> Right


makeParseArgs :: Name -> String -> Array Foreign
makeParseArgs (Name { user, project }) source =
  [ Foreign.toForeign [ user, project ]
  , Foreign.toForeign source
  ]


parseDependencies ::
  ∀  e
  .  Url
  -> Name
  -> String
  -> Task
      (compiler :: COMPILER | e)
      String 
      (Either (Array Error) { name :: Raw, dependencies :: Array Raw })
parseDependencies compilerUrl name source = do
  output <- exec compilerUrl "parse" (makeParseArgs name source)
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
                    , put name
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
  success <- input ! 0 >>= Foreign.readString <#> (_ == "false")
  case success of
    true ->
      { interface: _, js: _ }
        <$> (input ! 1 >>= get)
        <*> (input ! 2 >>= get)
        <#> Right

    false ->
      input ! 1
        >>= recoverErrors
        <#> Left


compile ::
  ∀  e
  .  Url
  -> Name
  -> String
  -> Map CanonicalModule Interface
  -> Task (compiler :: COMPILER | e) String (Either (Array Error) Result)
compile compilerUrl name source interfaces = do
  output <- exec compilerUrl "compile" (makeCompileArgs name source interfaces)
  recoverResult output
    |> runExcept
    |> lmap (map Foreign.renderForeignError >>> intercalate "\n" )
    |> Task.fromEither
