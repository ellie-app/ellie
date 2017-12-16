module Data.Url
  ( absolute
  , relative
  , crossOrigin
  , QueryParameter
  , string
  , int
  , Url
  ) where

import Ellie.Prelude
import Data.String as String
import Global (encodeURIComponent)


newtype Url =
  Url String


instance showUrl :: Show Url where
  show (Url url) = url


absolute :: Array String -> Array QueryParameter -> Url
absolute pathSegments parameters =
  Url $ "/" <> String.joinWith "/" pathSegments <> showQuery parameters


relative :: Array String -> Array QueryParameter -> Url
relative pathSegments parameters =
  Url $ String.joinWith "/" pathSegments <> showQuery parameters


crossOrigin :: String -> Array String -> Array QueryParameter -> Url
crossOrigin origin pathSegments parameters =
  Url $ origin <> "/" <> String.joinWith "/" pathSegments <> showQuery parameters


newtype QueryParameter =
  QueryParameter
    { key :: String
    , value :: String
    }


string :: String -> String -> QueryParameter
string key value =
  QueryParameter { key: encodeURIComponent key, value: encodeURIComponent value }


int :: String -> Int -> QueryParameter
int key int =
  QueryParameter { key: encodeURIComponent key, value: show int }


showQuery :: Array QueryParameter -> String
showQuery parameters =
  case parameters of
    [] -> ""
    _ -> "?" <> String.joinWith "&" (map showQueryPair parameters)


showQueryPair :: QueryParameter -> String
showQueryPair (QueryParameter { key, value }) =
  key <> "=" <> value
