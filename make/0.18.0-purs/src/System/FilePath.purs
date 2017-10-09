module System.FilePath where

import Data.Foldable (foldl)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Path (FilePath())
import Data.Char (Char(), charString)
import Data.String (charAt, drop, joinWith, length, split)


type FilePath = String


infixr 5 join as </>


join :: FilePath -> FilePath -> FilePath
join "" p                                = p
join p  ""                               = p
join p p' | hasTrailing p && absolute p' = p ++ drop 1 p'
join p p' | hasTrailing p                = p ++ p'
join p p' | absolute p'                  = p ++ p'
join p p'                                = p ++ "/" ++ p'


absolute :: FilePath -> Boolean
absolute p = (charString <$> charAt 0 p) == Just "/"


hasTrailing :: FilePath -> Boolean
hasTrailing p = (charString <$> charAt (length p - 1) p) == Just "/"


joinPath :: [FilePath] -> FilePath
joinPath ps = foldl (</>) "" $ nonEmpty ps


normalize :: FilePath -> FilePath
normalize p =
  split "/" p
    # nonEmpty
    # normalizeDots []
    # joinWith "/"
    # leading
    # trailing
  where
    leading :: FilePath -> FilePath
    leading p' | absolute p = "/" ++ p'
    leading p'              = p'

    trailing :: FilePath -> FilePath
    trailing p' | hasTrailing p  && length p > 1 = p' ++ "/"
    trailing p'                                  = p'

    normalizeDots :: [FilePath] -> [FilePath] -> [FilePath]
    normalizeDots acc []        = acc
    normalizeDots acc (".":ps)  = normalizeDots acc                       ps
    normalizeDots acc ("..":ps) = normalizeDots (fromMaybe [] $ init acc) ps
    normalizeDots acc (p:ps)    = normalizeDots (acc `snoc` p)            ps


nonEmpty :: [FilePath] -> [FilePath]
nonEmpty ps = filter ((/=) "") ps


basename :: FilePath -> FilePath
basename p =
  split "/" p
    # last
    # fromMaybe ""
