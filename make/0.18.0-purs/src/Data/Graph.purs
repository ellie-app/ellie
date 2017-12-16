module Data.Graph
  ( Graph
  , fromAdjacency
  , topologicalSort
  , nodes
  , dfsForest
  )
  where

import Ellie.Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Graph.Tree (Tree, Forest)
import Data.Graph.Tree as Tree
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Extra as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Partial


data Edge v
  = Edge v v


type GraphInternal v =
  Map v (Array v)


newtype Graph v =
  Graph (GraphInternal v)


fromAdjacency :: ∀ v. Map v (Array v) -> Graph v
fromAdjacency =
  Graph


nodes :: ∀ v. Graph v -> Array v
nodes (Graph graph) =
  graph
    |> Map.keys
    |> Array.fromFoldable


type SearchState v a =
  { visited :: Set v, output :: a }


data AcyclicGraph v
  = AcyclicGraph (Graph v) (Array v)


type Visitor v a =
  v -> a -> { value :: a, post :: a -> a }


guidedDfs ::
  ∀  v a
  .  Ord v
  => Visitor v a
  -> Array v
  -> a
  -> GraphInternal v
  -> { output :: a, graph :: GraphInternal v }
guidedDfs visitor seeds acc graph = go seeds acc graph
  where
    go seeds acc graph =
      case Array.uncons seeds of
        Nothing ->
          { output: acc, graph }

        Just { head: next, tail: seeds1 } ->
          case Map.lookup next graph of
            Nothing ->
              go seeds1 acc graph

            Just neighbors ->
              let
                { value: accAfterDiscovery, post: finishNode } =
                  visitor next acc

                { output: accBeforeFinish, graph: graph1 } =
                  go neighbors accAfterDiscovery (Map.delete next graph)

                accAfterFinish =
                  finishNode accBeforeFinish
              in
                go seeds1 accAfterFinish graph1


reverseEdges :: ∀ v. Ord v => GraphInternal v -> GraphInternal v
reverseEdges graph =
  graph
    |> Map.toArray
    |> Array.foldl
        (\result (Tuple vertex edges) ->
          Array.foldl
            (\stuff edge ->
              case Map.lookup edge stuff of
                Just reversedEdges -> Map.insert edge (vertex : reversedEdges) stuff
                Nothing -> Map.insert edge [vertex] stuff
            )
            result
            edges
        )
        Map.empty


dfs ::
  ∀  v a
  .  Ord v
  => Visitor v a
  -> a
  -> GraphInternal v
  -> a
dfs visitor initial graph =
  guidedDfs
    visitor
    (Map.keys graph |> Array.fromFoldable)
    initial
    graph
    |> _.output



checkForBackEdges ::
  ∀  v
  .  Ord v
  => Array v
  -> GraphInternal v
  -> Either (Edge v) (AcyclicGraph v)
checkForBackEdges ordering graph =
  let
    check vertex backSet =
      let
        backSetWithId = Set.insert vertex backSet
        edges = Map.unsafeLookup vertex graph
        backEdges = Set.intersection (Set.fromFoldable edges) backSetWithId
      in
        case Set.findMin backEdges of
          Nothing -> Right backSetWithId
          Just to -> Left (Edge vertex to)
  in
  ordering
      |> Array.foldl
          (\result vertex -> result >>= check vertex)
          (Right Set.empty)
      |> map (\_ -> AcyclicGraph (Graph graph) ordering)


onFinish :: ∀ a v. (v -> a -> a) -> Visitor v a
onFinish visitor vertex acc =
  { value: acc, post: visitor vertex }


type SccContext v =
  { vertex :: v, outbound :: Array v }


topologicalSort :: ∀ v. Ord v => Graph v -> Either (Array (Graph v)) (Array v)
topologicalSort (Graph graph) =
  let
    insertHere vertex other =
      Map.insert vertex (Map.unsafeLookup vertex graph) other

    reversePostOrder :: Array v
    reversePostOrder =
      dfs (onFinish (:)) [] graph
  in
    case checkForBackEdges reversePostOrder graph of
      Left _ ->
        graph
          |> reverseEdges
          |> Graph
          |> dfsForest reversePostOrder
          |> map (Tree.preOrderArray >>> Array.foldr insertHere Map.empty >>> reverseEdges >>> Graph)
          |> Left

      Right (AcyclicGraph graph ordering) ->
        Right ordering



dfsForest :: ∀ v. Ord v => Array v -> Graph v -> Forest v
dfsForest seeds (Graph graph) =
  let
    visitor ctx trees =
      { value: [], post: \children -> Tree.inner ctx children : trees }
  in
    guidedDfs visitor seeds [] graph
        |> _.output
        |> Array.reverse


dfsTree :: ∀ v. Ord v => v -> GraphInternal v -> Tree v
dfsTree seed graph =
  case dfsForest [ seed ] (Graph graph) of
    [] -> Tree.empty
    [ tree ] -> tree
    _ -> Partial.unsafeCrashWith "dfsTree: There can't be more than one DFS tree. This invariant is violated, please report this bug."
