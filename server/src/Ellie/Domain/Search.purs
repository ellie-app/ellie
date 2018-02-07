module Ellie.Domain.Search
  ( class Search
  , search
  ) where


import Ellie.Elm.Package.Searchable (Searchable)

class Search m where
  search :: String -> m (Array Searchable)


