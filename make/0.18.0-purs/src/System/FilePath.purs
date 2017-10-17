module System.FilePath where

import Prelude
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (charAt, drop, joinWith, length, split, singleton, Pattern(..))
import Data.Monoid ((<>))
import Data.Array ((:), init, snoc, filter, uncons, last)

type FilePath = String


infixr 5 join as </>


join :: FilePath -> FilePath -> FilePath
join "" p                                = p
join p  ""                               = p
join p p' | hasTrailing p && absolute p' = p <> drop 1 p'
join p p' | hasTrailing p                = p <> p'
join p p' | absolute p'                  = p <> p'
join p p'                                = p <> "/" <> p'


infixr 5 extension as <.>


extension :: FilePath -> String -> FilePath
extension p ext =
  p <> "." <> ext


absolute :: FilePath -> Boolean
absolute p = (singleton <$> charAt 0 p) == Just "/"


hasTrailing :: FilePath -> Boolean
hasTrailing p = (singleton <$> charAt (length p - 1) p) == Just "/"


joinPath :: Array FilePath -> FilePath
joinPath ps = foldl (</>) "" $ nonEmpty ps


normalize :: FilePath -> FilePath
normalize p =
  split (Pattern "/") p
    # nonEmpty
    # normalizeDots []
    # joinWith "/"
    # leading
    # trailing
  where
    leading :: FilePath -> FilePath
    leading p' = "/" <> p'

    trailing :: FilePath -> FilePath
    trailing p' | hasTrailing p  && length p > 1 = p' <> "/"
    trailing p'                                  = p'

    normalizeDots :: Array FilePath -> Array FilePath -> Array FilePath
    normalizeDots acc current =
      case uncons current of
        Nothing -> acc
        Just { head: ".", tail } -> normalizeDots acc tail
        Just { head: "..", tail } -> normalizeDots (fromMaybe [] $ init acc) tail
        Just { head, tail } -> normalizeDots (acc `snoc` head) tail


nonEmpty :: Array FilePath -> Array FilePath
nonEmpty ps = filter ((/=) "") ps


basename :: FilePath -> FilePath
basename p =
  split (Pattern "/") p
    # last
    # fromMaybe ""
