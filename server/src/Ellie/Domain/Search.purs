module Ellie.Domain.Search
  ( class Search
  , search
  ) where


import Elm.Package.Searchable (Searchable)

class Search m where
  search ∷ String → m (Array Searchable)
