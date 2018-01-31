module Data.FilePath
  ( FilePath
  , (</>)
  , joinPath
  , joinParts
  , (<.>)
  , extension
  , normalize
  , basename
  ) where


import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (Pattern(..))
import Data.String as String


type FilePath = String


infixr 5 joinPath as </>
joinPath :: FilePath -> FilePath -> FilePath
joinPath "" p                                = p
joinPath p  ""                               = p
joinPath p p' | hasTrailing p && absolute p' = p <> String.drop 1 p'
joinPath p p' | hasTrailing p                = p <> p'
joinPath p p' | absolute p'                  = p <> p'
joinPath p p'                                = p <> "/" <> p'


infixr 5 extension as <.>
extension :: FilePath -> String -> FilePath
extension p ext =
  p <> "." <> ext


absolute :: FilePath -> Boolean
absolute p = String.charAt 0 p == Just '/'


hasTrailing :: FilePath -> Boolean
hasTrailing p = String.charAt (String.length p - 1) p == Just '/'


joinParts :: Array FilePath -> FilePath
joinParts ps = Array.foldl (</>) "" $ nonEmpty ps


normalize :: FilePath -> FilePath
normalize p =
  String.split (Pattern "/") p
    # nonEmpty
    # normalizeDots []
    # String.joinWith "/"
    # leading
    # trailing
  where
    leading :: FilePath -> FilePath
    leading p' = "/" <> p'

    trailing :: FilePath -> FilePath
    trailing p' | hasTrailing p  && String.length p > 1 = p' <> "/"
    trailing p'                                  = p'

    normalizeDots :: Array FilePath -> Array FilePath -> Array FilePath
    normalizeDots acc current =
      case Array.uncons current of
        Nothing -> acc
        Just { head: ".", tail } -> normalizeDots acc tail
        Just { head: "..", tail } -> normalizeDots (Maybe.fromMaybe [] $ Array.init acc) tail
        Just { head, tail } -> normalizeDots (Array.snoc acc head) tail


nonEmpty :: Array FilePath -> Array FilePath
nonEmpty ps = Array.filter ((/=) "") ps


basename :: FilePath -> FilePath
basename p =
  String.split (Pattern "/") p
    # Array.last
    # Maybe.fromMaybe ""
