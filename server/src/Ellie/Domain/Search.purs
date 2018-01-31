module Ellie.Domain.Search
  ( class Search
  , search
  ) where


import Ellie.Elm.Package (Package)

class Search m where
  search :: String -> m (Array Package)


