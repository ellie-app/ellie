module Data.Url
  ( Url
  , protocol
  , username
  , password
  , auth
  , hostname
  , port
  , host
  , origin
  , pathname
  , search
  , hash
  , path
  , href
  , parse
  , query
  ) where


import Prelude
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Data.Url.Query (Query)
import Data.Url.Query as Query


foreign import _parse :: ∀ a. (a -> Maybe a) -> Maybe a -> String -> Url


newtype Url =
  Url
    { protocol :: Maybe String
    , username :: Maybe String
    , password :: Maybe String
    , hostname :: Maybe String
    , port :: Maybe Int
    , pathname :: String
    , query :: Query
    , hash :: Maybe String
    }
instance showUrl :: Show Url where show = href


query ∷ Url → Query
query (Url u) = u.query


parse :: String -> Url
parse = _parse Just Nothing


protocol :: Url -> Maybe String
protocol (Url u) = u.protocol


username :: Url -> Maybe String
username (Url u) = u.username


password :: Url -> Maybe String
password (Url u) = u.password


auth :: Url -> Maybe String
auth (Url u) =
  case Tuple u.username u.password of
    Tuple (Just username') (Just password') -> Just $ username' <> ":" <> password'
    Tuple (Just username') Nothing -> Just username'
    _ -> Nothing


hostname :: Url -> Maybe String
hostname (Url u) = u.hostname


port :: Url -> Maybe Int
port (Url u) = u.port


host :: Url -> Maybe String
host url =
  case Tuple (hostname url) (port url) of
    Tuple (Just hostname') (Just port') -> Just (hostname' <> ":" <> show port')
    Tuple (Just hostname') Nothing -> Just hostname'
    _ -> Nothing


origin :: Url -> Maybe String
origin url =
  case Tuple (protocol url) (host url) of
    Tuple (Just protocol') (Just host') -> Just $ protocol' <> "//" <> host'
    _ -> Nothing


pathname :: Url -> String
pathname (Url u) =
  u.pathname


search :: Url -> String
search (Url u) =
  if Query.isEmpty u.query then
    ""
  else
    "?" <> show u.query


hash :: Url -> Maybe String
hash (Url u) =
  u.hash


path :: Url -> String
path url =
  pathname url <> search url


href :: Url -> String
href url =
  protocol' <> auth' <> host' <> path' <> hash'
  where
    protocol' =
      url
        # protocol
        # map (_ <> "//")
        # Maybe.fromMaybe ""
    
    auth' =
      url
        # auth
        # map (_ <> "@")
        # Maybe.fromMaybe ""
    
    host' =
      url
        # host
        # Maybe.fromMaybe ""
    
    path' =
      path url
    
    hash' =
      url
        # hash
        # Maybe.fromMaybe ""
