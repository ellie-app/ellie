module Data.Dom.Parser where

import DOM.Node.Types (Document)

foreign import _parse ∷ String → Document

parse ∷ String → Document
parse = _parse
