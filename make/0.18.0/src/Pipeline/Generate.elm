module Pipeline.Generate exposing (..)

import Data.File as File exposing (File)
import Data.FilePath as FilePath exposing (FilePath)
import Data.HashDict as HashDict exposing (HashDict)
import Elm.Compiler.Module as Module
import Elm.Compiler.Module.Interface as Interface exposing (Interface)
import Elm.Make.CanonicalModule as CanonicalModule exposing (CanonicalModule)
import Elm.Make.Config as BM
import Elm.Make.Error as BM
import Elm.Make.Location as Location exposing (Location)
import Elm.Package.Name as Name exposing (Name)
import Extra.Graph as Graph
import Extra.List as List
import Extra.Task as Task
import FileStorage as FileStorage
import Graph exposing (Graph)
import Graph.Tree as Tree exposing (Forest, Tree)
import Json.Decode as Decode exposing (Decoder)
import Path as Path
import Set exposing (Set)
import Task exposing (Task)


-- GENERATE ELM STUFF


generate :
    BM.Config
    -> HashDict CanonicalModule Interface
    -> HashDict CanonicalModule (List CanonicalModule)
    -> HashDict CanonicalModule Location
    -> List CanonicalModule
    -> Task BM.Error (Maybe File)
generate config interfaces dependencies natives rootModules =
    case rootModules of
        [] ->
            Task.succeed Nothing

        _ ->
            let
                objectFiles =
                    setupNodes config.artifactDirectory dependencies natives
                        |> getReachableObjectFiles config.debug rootModules

                footer =
                    createFooter config.debug interfaces rootModules
            in
            objectFiles
                |> List.map
                    (\filePath ->
                        FileStorage.read filePath
                            |> Task.map (Decode.decodeValue Decode.string)
                            |> Task.andThen Task.fromResult
                            |> Task.mapError (\_ -> BM.CorruptedArtifact filePath)
                    )
                |> Task.sequence
                |> Task.map
                    (\files ->
                        (header :: (files ++ [ footer ]))
                            |> File.fromStringParts "out.js" "application/javascript"
                    )
                |> Task.map Just


setupNodes :
    FilePath
    -> HashDict CanonicalModule (List CanonicalModule)
    -> HashDict CanonicalModule Location
    -> List ( FilePath, CanonicalModule, List CanonicalModule )
setupNodes cachePath dependencies natives =
    let
        nativeNodes =
            natives
                |> HashDict.toList
                |> List.map (\( name, loc ) -> ( Path.toSource loc, name, [] ))

        dependencyNodes =
            dependencies
                |> HashDict.toList
                |> List.map (\( name, deps ) -> ( Path.toObjectFile cachePath name, name, deps ))
    in
    nativeNodes ++ dependencyNodes


getReachableObjectFiles :
    Bool
    -> List CanonicalModule
    -> List ( FilePath, CanonicalModule, List CanonicalModule )
    -> List FilePath
getReachableObjectFiles debug moduleNames allNodes =
    let
        nodes =
            if debug then
                allNodes
            else
                List.filter (not << isVirtualDomDebug) allNodes

        nodeToFilePath =
            nodes
                |> List.map (\( fp, cm, lcm ) -> ( cm, fp ))
                |> HashDict.fromList

        graph =
            nodes
                |> List.map (\( fp, cm, lcm ) -> ( cm, lcm ))
                |> Graph.fromEdges CanonicalModule.compare

        graphNodes =
            Graph.nodes graph

        keyToVertexDict =
            graphNodes
                |> List.map (\n -> ( n.label, n.id ))
                |> HashDict.fromList

        keyToVertex modul =
            HashDict.get modul keyToVertexDict

        reachableSet =
            moduleNames
                |> List.filterMap keyToVertex
                |> flip Graph.dfsForest graph
                |> List.concatMap Tree.levelOrderList
                |> List.map (.node >> .id)
                |> Set.fromList
    in
    Graph.topologicalSort graph
        |> Result.withDefault []
        |> List.filter (\ctx -> Set.member ctx.node.id reachableSet)
        |> List.reverse
        |> List.map (\ctx -> ctx.node.label)
        |> List.filterMap (\modul -> HashDict.get modul nodeToFilePath)


isVirtualDomDebug : ( fp, CanonicalModule, deps ) -> Bool
isVirtualDomDebug ( _, canonicalModule, _ ) =
    Tuple.first canonicalModule.package == Name.virtualDom && canonicalModule.name == [ "VirtualDom", "Debug" ]



-- FOOTER


createFooter :
    Bool
    -> HashDict CanonicalModule Interface
    -> List CanonicalModule
    -> String
createFooter debugMode interfaces rootModules =
    let
        exportChunks =
            List.map
                (exportProgram debugMode interfaces)
                (List.sortWith CanonicalModule.compare rootModules)
    in
    "var Elm = {};\n"
        ++ String.join "\n" exportChunks
        ++ footerClose


exportProgram :
    Bool
    -> HashDict CanonicalModule Interface
    -> CanonicalModule
    -> String
exportProgram debugMode interfaces canonicalName =
    let
        program =
            CanonicalModule.qualifiedVar canonicalName "main"

        object =
            objectFor canonicalName.name

        name =
            Module.nameToString canonicalName.name

        debugArg =
            "undefined"
    in
    setup canonicalName.name
        ++ "if (typeof "
        ++ program
        ++ " !== 'undefined') {\n    "
        ++ program
        ++ "("
        ++ object
        ++ ", '"
        ++ name
        ++ "', "
        ++ debugArg
        ++ ");\n}"


setup : List String -> String
setup moduleName =
    let
        create path =
            let
                jsPath =
                    objectFor path
            in
            jsPath ++ " = " ++ jsPath ++ " || {};"

        paths =
            moduleName
                |> List.inits
                |> List.tail
                |> Maybe.withDefault []
    in
    String.join "\n" (List.map create paths)


objectFor : Module.Raw -> String
objectFor names =
    let
        brackets name =
            "['" ++ name ++ "']"
    in
    "Elm" ++ String.join "" (List.map brackets names)


footerClose : String
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


header : String
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
