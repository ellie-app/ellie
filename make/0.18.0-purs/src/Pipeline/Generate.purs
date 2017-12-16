module Pipeline.Generate where

import Ellie.Prelude

import BuildManager (Task)
import BuildManager as BM
import Data.Array ((:))
import Data.Array (concatMap, filter, mapMaybe, reverse, sort, tail) as Array
import Data.Array.Extra (inits) as Array
import Data.Bifunctor (lmap)
import Data.Either.Extra (withDefault) as Either
import Data.Graph as Graph
import Data.Graph.Tree as Tree
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Map.Extra (toArray) as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Elm.Compiler.Module.Interface (Interface)
import Elm.Compiler.Module.Name.Raw (Raw(..))
import Elm.Package (Package(..))
import Elm.Package.Name as Name
import System.FileSystem (FilePath)
import System.FileSystem as FileSystem
import TheMasterPlan.CanonicalModule (CanonicalModule(..))
import TheMasterPlan.CanonicalModule as CanonicalModule
import TheMasterPlan.Location (Location)


-- GENERATE ELM STUFF


generate ::
  Map CanonicalModule Interface
  -> Map CanonicalModule (Array CanonicalModule)
  -> Map CanonicalModule Location
  -> Array CanonicalModule
  -> Task (Maybe String)
generate interfaces dependencies natives rootModules =
  case rootModules of
    [] ->
      pure Nothing

    _ -> do
      let
        objectFilePaths =
          getReachableObjectFiles rootModules <|
            setupNodes dependencies natives
      let
        footer =
          createFooter interfaces rootModules

      objectFiles <-
        traverse
          (\filePath ->
            lmap (\_ -> BM.CorruptedArtifact filePath) <|
              FileSystem.read filePath
          )
          objectFilePaths

      [ footer ]
        |> (objectFiles <> _)
        |> (header : _)
        |> String.joinWith ""
        |> Just
        |> pure


tupleToNativeNode :: Tuple CanonicalModule Location -> Node
tupleToNativeNode (Tuple name loc) =
  { path: loc |> unwrap |> _.relativePath
  , name
  , deps: []
  }


tupleToElmNode :: Tuple CanonicalModule (Array CanonicalModule) -> Node
tupleToElmNode (Tuple name deps) =
  { path: CanonicalModule.objectPath name
  , name
  , deps
  }

type Node =
  { path :: FilePath
  , name :: CanonicalModule
  , deps :: Array CanonicalModule
  }

setupNodes ::
  Map CanonicalModule (Array CanonicalModule)
  -> Map CanonicalModule Location
  -> Array Node
setupNodes dependencies natives =
  let
    nativeNodes =
      natives
        |> Map.toArray
        |> map tupleToNativeNode

    dependencyNodes =
      dependencies
        |> Map.toArray
        |> map tupleToElmNode
  in
    nativeNodes <> dependencyNodes


getReachableObjectFiles ::
  Array CanonicalModule
  -> Array Node
  -> Array FilePath
getReachableObjectFiles moduleNames allNodes =
  let
    nodes =
      Array.filter (not <<< isVirtualDomDebug) allNodes

    nodeToFilePath =
      nodes
        |> map (\{ path, name, deps } -> Tuple name path)
        |> Map.fromFoldable

    graph =
      nodes
        |> map (\{ path, name, deps } -> Tuple name deps)
        |> Map.fromFoldable
        |> Graph.fromAdjacency

    graphNodes =
      Graph.nodes graph

    reachableSet =
        moduleNames
          |> flip Graph.dfsForest graph
          |> Array.concatMap Tree.levelOrderArray
          |> Set.fromFoldable
    in
      Graph.topologicalSort graph
        |> Either.withDefault []
        |> Array.filter (flip Set.member reachableSet)
        |> Array.reverse
        |> Array.mapMaybe (flip Map.lookup nodeToFilePath)


isVirtualDomDebug :: Node -> Boolean
isVirtualDomDebug { name: CanonicalModule { package: Package p, name} } =
  p.name == Name.virtualDom
    && ( name == Raw [ "VirtualDom", "Debug" ])



-- FOOTER


createFooter ::
  Map CanonicalModule Interface
  -> Array CanonicalModule
  -> String
createFooter interfaces rootModules =
  let
    exportChunks =
      rootModules
        |> Array.sort
        |> map (exportProgram interfaces)
  in
    "var Elm = {};\n"
      <> String.joinWith "\n" exportChunks
      <> footerClose


exportProgram ::
  Map CanonicalModule Interface
  -> CanonicalModule
  -> String
exportProgram interfaces canonicalName =
  let
    program =
      CanonicalModule.qualifiedVar canonicalName "main"

    object =
      canonicalName
        |> unwrap
        |> _.name
        |> unwrap
        |> objectFor

    name =
      canonicalName
        |> unwrap
        |> _.name
        |> show

    debugArg =
      "undefined"
  in
    setup (unwrap canonicalName |> _.name)
      <> "if (typeof "
      <> program
      <> " !== 'undefined') {\n    "
      <> program
      <> "("
      <> object
      <> ", '"
      <> name
      <> "', "
      <> debugArg
      <> ");\n}"


setup :: Raw -> String
setup moduleName =
  let
    create path =
      let
        jsPath =
          objectFor path
      in
        jsPath <> " = " <> jsPath <> " || {};"

    paths =
      moduleName
        |> unwrap
        |> Array.inits
        |> Array.tail
        |> Maybe.fromMaybe []
  in
    String.joinWith "\n" (map create paths)


objectFor :: Array String -> String
objectFor names =
  let
    brackets name =
      "['" <> name <> "']"
  in
    names
      |> map brackets
      |> String.joinWith ""
      |> ("Elm" <> _)


footerClose :: String
footerClose =
    """
if (typeof define === "function" && define['amd'])
{
  define([], function() { return Elm; });
  return;
}

if (typeof module === "object")
{
  module['exports'] = Elm;
  return;
}

var globalElm = this['Elm'];
if (typeof globalElm === "undefined")
{
  this['Elm'] = Elm;
  return;
}

for (var publicModule in Elm)
{
  if (publicModule in globalElm)
  {
    throw new Error('There are two Elm modules called `' + publicModule + '` on this page! Rename one of them.');
  }
  globalElm[publicModule] = Elm[publicModule];
}

}).call(this);
"""



-- HEADER


header :: String
header =
    """
(function() {
'use strict';

function F2(fun)
{
  function wrapper(a) { return function(b) { return fun(a,b); }; }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun)
{
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun)
{
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun, a, b)
{
  return fun.arity === 2
    ? fun.func(a, b)
    : fun(a)(b);
}
function A3(fun, a, b, c)
{
  return fun.arity === 3
    ? fun.func(a, b, c)
    : fun(a)(b)(c);
}
function A4(fun, a, b, c, d)
{
  return fun.arity === 4
    ? fun.func(a, b, c, d)
    : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e)
{
  return fun.arity === 5
    ? fun.func(a, b, c, d, e)
    : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f)
{
  return fun.arity === 6
    ? fun.func(a, b, c, d, e, f)
    : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g)
{
  return fun.arity === 7
    ? fun.func(a, b, c, d, e, f, g)
    : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h)
{
  return fun.arity === 8
    ? fun.func(a, b, c, d, e, f, g, h)
    : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i)
{
  return fun.arity === 9
    ? fun.func(a, b, c, d, e, f, g, h, i)
    : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}
"""
