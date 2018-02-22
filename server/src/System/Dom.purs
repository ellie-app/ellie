module System.Dom
  ( class IsParentNode
  , querySelectorAll
  , prepend
  , class IsNode
  , getTextContent
  , setTextContent
  , appendChild
  , Document
  , createElement
  , parseFromString
  , head
  , body
  , documentElement
  , Element
  , getAttribute
  , setAttribute
  , setInnerHtml
  , getOuterHtml
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff) as Eff
import Control.Monad.IO (IO)
import Control.Monad.IO.Effect (INFINITY)
import Data.Maybe (Maybe(..))


foreign import data Document ∷ Type
foreign import _parseFromString ∷ String → Eff (infinity ∷ INFINITY) Document
foreign import _documentQuerySelectorAll ∷ { selector ∷ String, document ∷ Document } → Eff (infinity ∷ INFINITY) (Array Element)
foreign import _documentCreateElement ∷ { tagName ∷ String, document ∷ Document } → Eff (infinity ∷ INFINITY) Element
foreign import _documentPrepend ∷ { document ∷ Document, child ∷ Element, helpers ∷ FfiHelpers } → Eff (infinity ∷ INFINITY) Unit
foreign import _documentHead ∷ Document → Eff (infinity ∷ INFINITY) Element
foreign import _documentBody ∷ Document → Eff (infinity ∷ INFINITY) Element
foreign import _documentDocumentElement ∷ Document → Eff (infinity ∷ INFINITY) Element
foreign import data Element ∷ Type
foreign import _elementGetAttribute ∷ { name ∷ String, element ∷ Element, helpers ∷ FfiHelpers } → Eff (infinity ∷ INFINITY) (Maybe String)
foreign import _elementSetAttribute ∷ { name ∷ String, value ∷ String, element ∷ Element, helpers ∷ FfiHelpers } → Eff (infinity ∷ INFINITY) Unit
foreign import _elementGetTextContent ∷ Element → Eff (infinity ∷ INFINITY) String
foreign import _elementSetTextContent ∷ { content ∷ String, element ∷ Element, helpers ∷ FfiHelpers } → Eff (infinity ∷ INFINITY) Unit
foreign import _elementSetInnerHtml ∷ { element ∷ Element, html ∷ String, helpers ∷ FfiHelpers } → Eff (infinity ∷ INFINITY) Unit
foreign import _elementPrepend ∷ { parent ∷ Element, child ∷ Element, helpers ∷ FfiHelpers } → Eff (infinity ∷ INFINITY) Unit
foreign import _elementQuerySelectorAll ∷ { selector ∷ String, element ∷ Element } → Eff (infinity ∷ INFINITY) (Array Element)
foreign import _elementGetOuterHtml ∷ Element → Eff (infinity ∷ INFINITY) String
foreign import _elementAppendChild ∷ { parent ∷ Element, child ∷ Element, helpers ∷ FfiHelpers } → Eff (infinity ∷ INFINITY) Unit


type FfiHelpers =
  { just ∷ ∀ a. a → Maybe a
  , nothing ∷ ∀ a. Maybe a
  , unit ∷ Unit
  }


helpers ∷ FfiHelpers
helpers =
  { just: Just
  , nothing: Nothing
  , unit: unit
  }


class IsParentNode a where
  querySelectorAll ∷ String → a → IO (Array Element)
  prepend ∷ Element → a → IO Unit 


class IsNode a where
  getTextContent ∷ a → IO String
  setTextContent ∷ String → a → IO Unit
  appendChild ∷ a → a → IO Unit


-- DOCUMENT


instance isParentNodeDocument ∷ IsParentNode Document where
  querySelectorAll selector document = Eff.liftEff $ _documentQuerySelectorAll { selector, document }
  prepend child document = Eff.liftEff $ _documentPrepend { child, document, helpers }


parseFromString ∷ String → IO Document
parseFromString input =
  Eff.liftEff $ _parseFromString input


createElement ∷ String → Document → IO Element
createElement tagName document =
  Eff.liftEff $ _documentCreateElement { tagName, document }


head ∷ Document → IO Element
head document =
  Eff.liftEff $ _documentHead document


body ∷ Document → IO Element
body document =
  Eff.liftEff $ _documentBody document


documentElement ∷ Document → IO Element
documentElement document =
  Eff.liftEff $ _documentDocumentElement document


-- ELEMENT


instance isParentNodeElement ∷ IsParentNode Element where
  querySelectorAll selector element = Eff.liftEff $ _elementQuerySelectorAll { selector, element }
  prepend child parent = Eff.liftEff $ _elementPrepend { child, parent, helpers }


instance isNodeElement ∷ IsNode Element where
  getTextContent element = Eff.liftEff $ _elementGetTextContent element
  setTextContent content element = Eff.liftEff $ _elementSetTextContent { content, element, helpers }
  appendChild child parent = Eff.liftEff $ _elementAppendChild { child, parent, helpers }


getAttribute ∷ String → Element → IO (Maybe String)
getAttribute name element =
  Eff.liftEff $ _elementGetAttribute { helpers, name, element }


setAttribute ∷ String → String → Element → IO Unit
setAttribute name value element =
  Eff.liftEff $ _elementSetAttribute { helpers, name, value, element }


setInnerHtml ∷ String → Element → IO Unit
setInnerHtml html element =
  Eff.liftEff $ _elementSetInnerHtml { html, element, helpers }


getOuterHtml ∷ Element → IO String
getOuterHtml element =
  Eff.liftEff $ _elementGetOuterHtml element
