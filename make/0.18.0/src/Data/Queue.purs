module Data.Queue (Queue, empty, insert, remove) where

import Ellie.Prelude
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))


data Queue a
    = Queue (List a) (List a)


empty :: ∀ a. Queue a
empty =
  Queue Nil Nil


insert :: ∀ a. a -> Queue a -> Queue a
insert a (Queue front back) =
  Queue front (Cons a back)


remove :: ∀ a. Queue a -> { value :: Maybe a, rest :: Queue a }
remove (Queue Nil Nil) = { value: Nothing, rest: empty }
remove (Queue Nil back) = remove <| Queue (List.reverse back) Nil
remove (Queue (Cons next rest) back) = { value: Just next, rest: Queue rest back }
