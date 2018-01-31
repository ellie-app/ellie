module Data.Url.Parser
  ( Parser
  , string
  , int
  , s
  , (</>)
  , segment
  , top
  , custom
  , parse
  , collect
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Url (Url)
import Data.Url as Url
import Data.String (Pattern(..))
import Data.String as String


-- PARSERS


{-| Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data.
-}
newtype Parser a b =
  Parser (State a -> Array (State b))


type State a =
  { visited :: Array String
  , unvisited :: Array String
  , value :: a
  }


string :: ∀ a. Parser (String -> a) a
string =
  custom "STRING" Just


int :: ∀ a. Parser (Int -> a) a
int =
  custom "NUMBER" Int.fromString


s :: ∀ a. String -> Parser a a
s str =
  Parser $ \{ visited, unvisited, value } ->
    case Array.uncons unvisited of
      Nothing -> []

      Just { head: next, tail: rest } ->
        if next == str then
          [ { visited: next : visited, unvisited: rest, value } ]
        else
          []


custom :: ∀ a b. String -> (String -> Maybe a) -> Parser (a -> b) b
custom tipe stringToSomething =
  Parser $ \{ visited, unvisited, value } ->
    case Array.uncons unvisited of
      Nothing ->
        []

      Just { head: next, tail: rest } ->
        case stringToSomething next of
          Just nextValue ->
            [ { visited : next : visited, unvisited: rest, value: value nextValue } ]

          Nothing ->
            []


infixr 7 segment as </>

segment :: ∀ a b c. Parser a b -> Parser b c -> Parser a c
segment (Parser parseBefore) (Parser parseAfter) =
  Parser $ \state ->
    Array.concatMap parseAfter (parseBefore state)


collect :: ∀ a b c. a -> Parser a c -> Parser (c -> b) b
collect subvalue (Parser parse) =
  Parser $ \{ visited, unvisited, value } ->
    map (mapState value) $ parse $
      { visited, unvisited, value: subvalue }


mapState :: ∀ a b. (a -> b) -> State a -> State b
mapState func state =
  state { value = func state.value }


top :: ∀ a. Parser a a
top =
  Parser $ \state -> [state]

oneOf :: ∀ a b. Array (Parser a b) -> Parser a b
oneOf parsers =
  Parser $ \state ->
    Array.concatMap (\(Parser parser) -> parser state) parsers


parse :: ∀ a. Parser (a -> a) a -> Url -> Maybe a
parse (Parser parser) url  =
  getFirstMatch $ parser $
    { visited: []
    , unvisited: preparePath $ Url.path url
    , value: id
    }


getFirstMatch :: ∀ a. Array (State a) -> Maybe a
getFirstMatch states =
  case Array.uncons states of
    Nothing ->
      Nothing

    Just { head: state, tail: rest } ->
      case state.unvisited of
        [] -> Just state.value
        [""] -> Just state.value
        _ -> getFirstMatch rest


preparePath :: String -> Array String
preparePath path =
  let
    segments = String.split (Pattern "/") path
  in
    case Array.uncons segments of
      Just { head: "", tail } -> removeFinalEmpty tail
      _ -> removeFinalEmpty segments


removeFinalEmpty :: Array String -> Array String
removeFinalEmpty segments =
  case Array.uncons segments of
    Nothing -> []
    Just { head: "", tail: [] } -> []
    Just { head, tail } -> head : (removeFinalEmpty tail)
