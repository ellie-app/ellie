module Data.Graph.Tree where

import Ellie.Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Queue (Queue)
import Data.Queue as Queue

newtype Tree v =
  Tree
    { index :: Int
    , children :: Maybe { vertex :: v, children :: Forest v }
    }

arrayForTraversal traversal tree =
  let
    f vertex children rest =
      (vertex : _) >>> rest
    in
      traversal f id tree []


levelOrder :: ∀ a v. (v -> Forest v -> a -> a) -> a -> Tree v -> a
levelOrder visit acc tree = go acc (Queue.empty |> Queue.insert tree)
  where
    go acc toVisit =
      case Queue.remove toVisit of
        { value: Nothing } ->
          acc

        { value: Just tree, rest: othersToVisit } ->
            case root tree of
              Nothing ->
                go acc othersToVisit

              Just { vertex, children } ->
                go (visit vertex children acc) (pushMany children othersToVisit)


levelOrderArray :: ∀ v. Tree v -> Array v
levelOrderArray =
  arrayForTraversal levelOrder


pushMany :: ∀ a. Array a -> Queue a -> Queue a
pushMany vals queue =
    Array.foldl (flip Queue.insert) queue vals


preOrder :: ∀ a v. (v -> Forest v -> a -> a) -> a -> Tree v -> a
preOrder visitor acc tree =
  let
    fold =
      preOrder visitor
  in
    case root tree of
      Nothing ->
        acc
      Just { vertex, children } ->
        Array.foldl fold (visitor vertex children acc) children


preOrderArray :: ∀ v. Tree v -> Array v
preOrderArray =
  arrayForTraversal preOrder


type Forest v =
  Array (Tree v)


isEmpty :: ∀ v. Tree v -> Boolean
isEmpty (Tree { index: 0, children: Nothing }) = true
isEmpty _ = false


size :: ∀ v. Tree v -> Int
size (Tree { index }) =
  index


empty :: ∀ v. Tree v
empty =
    Tree { index: 0, children: Nothing }


inner :: ∀ v. v -> Array (Tree v) -> Tree v
inner vertex children =
  let
    nonEmptyChildren = Array.filter (not <<< isEmpty) children
    totalSize = Array.foldl (\a t -> size t + a) 1 nonEmptyChildren
  in
    Tree { index: totalSize, children: Just { vertex, children: nonEmptyChildren } }


root :: ∀ v. Tree v -> Maybe { vertex :: v, children :: Forest v }
root (Tree { children }) = children


leaf :: ∀ v. v -> Tree v
leaf val =
    inner val []
