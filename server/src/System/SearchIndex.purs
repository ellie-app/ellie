module System.SearchIndex
  ( SearchIndex
  , create
  , add
  , search
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class as Aff
import Control.Monad.Aff.Compat (EffFnAff)
import Control.Monad.Aff.Compat (fromEffFnAff) as Aff
import Control.Monad.Eff (kind Effect)
import Control.Monad.IO (IO)
import Data.Foreign (Foreign)
import Data.Foreign as Foreign
import Data.Function.Uncurried (Fn2, Fn3)
import Data.Function.Uncurried (runFn3, runFn2) as Function
import Data.Indexable (class Indexable)
import Data.Indexable as Indexable

foreign import data SEARCHINDEX :: Effect
foreign import data SearchIndex :: Type -> Type
foreign import _create :: ∀ e a. EffFnAff (searchIndex :: SEARCHINDEX | e) (SearchIndex a)
foreign import _add :: ∀ e a. Fn3 Unit (SearchIndex a) (Array Foreign) (EffFnAff (searchIndex :: SEARCHINDEX | e) Unit)
foreign import _search :: ∀ e r a. Fn3 (SearchIndex a) String Int (EffFnAff (searchIndex :: SEARCHINDEX | e) (Array Foreign))


create :: ∀ a. IO (SearchIndex a)
create =
  Aff.liftAff $ Aff.fromEffFnAff _create


add :: ∀ r a. Indexable r a => SearchIndex a -> Array a -> IO Unit
add index documents =
  Aff.liftAff $
    Aff.fromEffFnAff $
    Function.runFn3
      _add
      unit
      index
      (map (Foreign.toForeign <<< Indexable.toDocument) documents)


search :: ∀ r a. Indexable r a => SearchIndex a -> String -> Int -> IO (Array a)
search index query count =
  Aff.liftAff $
    Function.runFn3 _search index query count
      # Aff.fromEffFnAff
      # map (map (Foreign.unsafeFromForeign >>> Indexable.fromDocument))