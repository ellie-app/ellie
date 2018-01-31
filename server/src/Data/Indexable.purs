module Data.Indexable
  ( class Indexable
  , class IndexableData
  , class IndexableFields
  , toDocument
  , fromDocument
  ) where

import Type.Row (kind RowList, class RowToList, Nil, Cons)

class IndexableFields (r :: RowList)
instance allowedIndexFieldsCons :: IndexableFields t => IndexableFields (Cons l String t)
instance allowedIndexFieldsNil :: IndexableFields Nil


class IndexableData a
instance indexableRecord :: (RowToList r fields, IndexableFields fields) => IndexableData (Record r)


class IndexableData r ⇐ Indexable r a | a -> r where
  toDocument :: a -> r
  fromDocument :: r -> a