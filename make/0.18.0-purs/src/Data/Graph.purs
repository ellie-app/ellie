module Data.Graph
  ( Graph
  , nodes
  , dfsForest
  , topologicalSort
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Graph.Tree (Forest)
import Data.Graph.Tree as Tree
import Data.Map (Map)
import Data.Map (delete, empty, insert, keys, lookup) as Map
import Data.Map.Extra (insertWith, unsafeLookup) as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple


type Graph v = 
  Map v (Array v)

nodes :: ∀ v. Ord v => Graph v -> Array v
nodes graph =
  Array.fromFoldable $ Map.keys graph


data AcyclicGraph v =
  AcyclicGraph (Graph v) (Array v)


data Edge v
  = Edge v v


checkForBackEdges :: ∀ v. Ord v => Array v -> Graph v -> Either (Edge v) (AcyclicGraph v)
checkForBackEdges ordering graph =
  let
    check vertex (Tuple backSet _) =
      let
          backSetWithVertex = Set.insert vertex backSet
          outgoing = Set.fromFoldable (Map.unsafeLookup vertex graph)
          backEdges = Set.intersection outgoing backSetWithVertex
      in
        case Set.findMin backEdges of
            Nothing -> Right (Tuple backSetWithVertex unit)
            Just to -> Left (Edge vertex to)

    success _ = AcyclicGraph graph ordering
  in
    ordering
      # Array.foldl
          (\res vertex -> res >>= (check vertex))
          (Right (Tuple Set.empty unit))
      # map success


checkAcyclic :: ∀ v. Ord v => Graph v -> Either (Edge v) (AcyclicGraph v)
checkAcyclic graph =
  let reversePostOrder = dfs (onFinish (:)) [] graph
  in checkForBackEdges reversePostOrder graph


reverseEdges :: ∀ v. Ord v => Graph v -> Graph v
reverseEdges graph =
  let
    insert k v reversedGraph =
      Map.insertWith (flip (<>)) v [ k ] reversedGraph

    flipEdges k vs reversedGraph =
      Array.foldr (insert k) reversedGraph vs
  in
    foldrWithIndex flipEdges Map.empty graph


type SimpleNodeVisitor v acc =
  v -> acc -> acc


type DfsNodeVisitor v acc =
  v -> acc -> Tuple acc (acc -> acc)


onDiscovery :: ∀ v acc. Ord v => SimpleNodeVisitor v acc -> DfsNodeVisitor v acc
onDiscovery visitor vertex acc =
  Tuple (visitor vertex acc) id


onFinish :: ∀ v acc. Ord v => SimpleNodeVisitor v acc -> DfsNodeVisitor v acc
onFinish visitor vertex acc =
  Tuple acc (visitor vertex)


guidedDfs ::
  ∀  v acc
  .  Ord v
  => DfsNodeVisitor v acc
  -> Array v
  -> acc
  -> Graph v
  -> Tuple acc (Graph v)
guidedDfs visitNode seeds acc graph = go seeds acc graph
  where
    go seeds acc graph =
      case Array.uncons seeds of
        Nothing -> Tuple acc graph
        Just { head: next, tail: seeds1 } ->
          case Map.lookup next graph of
            Nothing ->
              go seeds1 acc graph
            
            Just outbound ->
              let
                (Tuple accAfterDiscovery finishNode) = visitNode next acc
                (Tuple accBeforeFinish graph1) = go outbound accAfterDiscovery (Map.delete next graph)
                accAfterFinish = finishNode accBeforeFinish
              in
                go seeds1 accAfterFinish graph1


dfs :: ∀ v acc. Ord v => DfsNodeVisitor v acc -> acc -> Graph v -> acc
dfs visitNode acc graph =
  Tuple.fst $ guidedDfs visitNode (nodes graph) acc graph



dfsForest :: ∀ v. Ord v => Array v -> Graph v -> Forest v
dfsForest seeds graph =
  let
    visitNode ctx trees =
      Tuple [] (\children -> Tree.inner ctx children : trees)
  in
    guidedDfs visitNode seeds [] graph
      # Tuple.fst
      # Array.reverse


topologicalSort :: ∀ v. Ord v => Graph v -> Either (Array (Graph v)) (Array v)
topologicalSort graph =
  let
    reversePostOrder = dfs (onFinish (:)) [] graph
  in
    case checkForBackEdges reversePostOrder graph of
      Right (AcyclicGraph _ ordering) ->
        Right ordering
      
      Left _ ->
        let
          reversed = reverseEdges graph
          forest = dfsForest reversePostOrder reversed
          insert vertex other = Map.insert vertex (Map.unsafeLookup vertex reversed) other
        in
          Left $ map (Tree.preOrderArray >>> Array.foldr insert Map.empty >>> reverseEdges) forest
