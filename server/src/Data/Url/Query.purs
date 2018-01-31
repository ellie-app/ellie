module Data.Url.Query where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.StrMap (StrMap)
import Data.StrMap as StrMap


newtype Query =
  Query (StrMap (Array String))


isEmpty :: Query -> Boolean
isEmpty (Query items) =
  StrMap.isEmpty items


add :: String -> String -> Query -> Query
add key value (Query entries) =
  Query $
    StrMap.alter
      (Maybe.fromMaybe [] >>> Array.cons value >>> Just)
      key
      entries

get ∷ String → Query → Maybe String
get key (Query query) =
  StrMap.lookup key query
    >>= Array.head


instance showQuery :: Show Query where
  show (Query entries) =
    String.joinWith "&" $
      StrMap.toArrayWithKey
        (\key value ->
          String.joinWith "&" $ map (\v -> key <> "=" <> v) value
        )
        entries