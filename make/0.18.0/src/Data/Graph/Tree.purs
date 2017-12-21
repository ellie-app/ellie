module Data.Graph.Tree
  ( Forest
  , Tree
  , empty
  , height
  , inner
  , isEmpty
  , leaf
  , levelOrder
  , levelOrderArray
  , postOrder
  , postOrderArray
  , preOrder
  , preOrderArray
  , root
  , size
  , unfoldForest
  , unfoldTree
  ) where

import Prelude
import Data.Array ((:))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Queue (Queue)
import Data.Queue as Queue

data Tree a =
  MkTree Int (Maybe { label :: a, children :: Forest a })


type Forest a =
  Array (Tree a)


empty :: ∀ a. Tree a
empty =
  MkTree 0 Nothing


leaf :: ∀ a. a -> Tree a
leaf val =
  inner val []


inner :: ∀ a. a -> Forest a -> Tree a
inner label children =
  let
    nonEmptyChildren = Array.filter (not <<< isEmpty) children
    totalSize = Array.foldl (\out tree -> out + size tree) 1 nonEmptyChildren
  in
  MkTree totalSize (Just { label, children: nonEmptyChildren })


unfoldTree ::
  ∀  label seed
  .  (seed -> { label :: label, children :: Array seed })
  -> seed
  -> Tree label
unfoldTree next seed =
  let { label, children: seeds } = next seed
  in inner label (map (unfoldTree next) seeds)


unfoldForest ::
  ∀  seed label
  .  (seed -> { label :: label, children :: Array seed })
  -> Array seed
  -> Forest label
unfoldForest next seeds =
  map (unfoldTree next) seeds


isEmpty :: ∀ label. Tree label -> Boolean
isEmpty (MkTree 0 Nothing) = true
isEmpty _ = false


root :: ∀ label. Tree label -> Maybe { label :: label, children :: Forest label }
root (MkTree _ maybe) = maybe


size :: ∀ label. Tree label -> Int
size (MkTree n _) = n


height :: ∀ label. Tree label -> Int
height tree = go 0 tree
  where
    go h t =
      case root t of
        Just { children } -> Array.foldl (\out item -> max (go (h + 1) item) out) (h + 1) children
        Nothing -> h
    


arrayForTraversal ::
  ∀  a b c d e f g h
  .  Category e
  => ((d -> b -> (Array d -> c) -> Array d -> c) -> e f f -> a -> Array g -> h)
  -> a
  -> h
arrayForTraversal traversal tree =
  let
    f label children rest = (label : _ ) >>> rest
    acc = id
  in
    traversal f acc tree []


pushMany :: ∀ a. Array a -> Queue a -> Queue a
pushMany vals queue =
    Array.foldl (flip Queue.insert) queue vals


levelOrder ::
  ∀  label acc
  .  (label -> Forest label -> acc -> acc)
  -> acc
  -> Tree label
  -> acc
levelOrder visit acc tree = go acc (Queue.insert tree Queue.empty)
  where
    go acc toVisit =
      case Queue.remove toVisit of
        { value: Nothing } -> acc
        { value: Just tree, rest } ->
          case root tree of
            Nothing ->
              go acc rest
            Just { label, children } ->
              go (visit label children acc) (pushMany children rest)


levelOrderArray :: ∀ label. Tree label -> Array label
levelOrderArray =
  arrayForTraversal levelOrder


postOrder ::
  ∀  label acc
  .  (label -> Forest label -> acc -> acc)
  -> acc
  -> Tree label
  -> acc
postOrder visit acc tree =
  let
    folder = postOrder visit
  in
    case root tree of
      Nothing -> acc
      Just { label, children } -> visit label children (Array.foldl folder acc children)



postOrderArray :: ∀ label. Tree label -> Array label
postOrderArray =
  arrayForTraversal postOrder


preOrder ::
  ∀  label acc
  .  (label -> Forest label -> acc -> acc)
  -> acc
  -> Tree label
  -> acc
preOrder visit acc tree =
  let
    folder =  preOrder visit
  in
    case root tree of
      Nothing -> acc
      Just { label, children } -> Array.foldl folder (visit label children acc) children


preOrderArray :: ∀ label. Tree label -> Array label
preOrderArray =
  arrayForTraversal preOrder