module System.FileSystem
  ( FILESYSTEM
  , Error(..)
  , Details(..)
  , read
  , exists
  , modified
  , remove
  , isOlderThan
  , write
  , (</>)
  , joinPath
  , joinParts
  , (<.>)
  , extension
  , FilePath
  ) where

import Ellie.Prelude

import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Now as Now
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Task (kind Effect, EffFnTask, FfiHelpers, Task)
import Control.Monad.Task as Task
import Data.Array (filter, init, intercalate, last, snoc, uncons)
import Data.Bifunctor (lmap)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Foldable (foldl)
import Data.Foreign (Foreign, MultipleErrors)
import Data.Foreign (renderForeignError) as Foreign
import Data.Foreign.Class (class Foreignable)
import Data.Foreign.Class (get, put) as Foreign
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (charAt, drop, joinWith, length, split, singleton, Pattern(..))
import Data.Time.Duration (class Duration, Milliseconds)
import Data.Time.Duration as Duration

foreign import data FILESYSTEM :: Effect
foreign import data RawError :: Type

data Details
  = FileNotFound
  | CorruptModifiedTime
  | ForeignError MultipleErrors
  | UnknownRuntimeCrash Exception.Error


newtype Error =
  Error
    { path :: FilePath
    , details :: Details
    }

derive instance newtypeError :: Newtype Error _


instance showError :: Show Error where
  show (Error error) =
    case error of
      { path, details: FileNotFound } -> "No file at path `" <> path <> "`"
      { path, details: CorruptModifiedTime } -> "Invalid modified time for path `" <> path <> "`"
      { path, details: ForeignError errors } -> "Could not read Foreign value at path `" <> path <> "`: " <> (intercalate "," (map Foreign.renderForeignError errors))
      { path, details: UnknownRuntimeCrash error } -> "Something failed at runtime while reading path `" <> path <> "`: " <> Exception.message error

foreign import _liftRawError ::
  Fn3
    (FilePath -> Error)
    (FilePath -> Error)
    RawError
    Error

liftRawError :: RawError -> Error
liftRawError raw =
  runFn3
    _liftRawError
    ({ details: FileNotFound, path: _ } >>> Error)
    ({ details: CorruptModifiedTime, path: _ } >>> Error)
    raw


foreign import _read ::
  ∀ e
  . Fn2
      FfiHelpers
      FilePath
      (EffFnTask (fileSystem :: FILESYSTEM | e) RawError Foreign)

read ::
  ∀  a e
  .  Foreignable a
  => FilePath
  -> Task (fileSystem :: FILESYSTEM | e) Error a
read path = do
  value <-
    path
        |> normalize
        |> runFn2 _read Task.ffiHelpers
        |> Task.fromEffFnTask "FileSystem.read"
        |> lmap liftRawError

  value
    |> Foreign.get
    |> Task.fromExcept
    |> lmap (ForeignError >>> { path, details: _ } >>> Error)


foreign import _write ::
  ∀ x e
  . Fn3
      FfiHelpers
      FilePath
      Foreign
      (EffFnTask (fileSystem :: FILESYSTEM | e) x Unit)

write ::
  ∀  e x a
  .  Foreignable a
  => FilePath
  -> a
  -> Task (fileSystem :: FILESYSTEM | e) x Unit
write path value =
  value
    |> Foreign.put
    |> runFn3 _write Task.ffiHelpers (normalize path)
    |> Task.fromEffFnTask "FileSystem.write"


foreign import _exists ::
  ∀ x e
  . Fn2
      FfiHelpers
      FilePath
      (EffFnTask (fileSystem :: FILESYSTEM | e) x Boolean)

exists ::
  ∀  x e
  .  FilePath
  -> Task (fileSystem :: FILESYSTEM | e) x Boolean
exists path =
  path
    |> normalize
    |> runFn2 _exists Task.ffiHelpers
    |> Task.fromEffFnTask "FileSystem.exists"


foreign import _modified ::
  ∀ e
  . Fn3
      FfiHelpers
      (Milliseconds -> Maybe Instant)
      FilePath
      (EffFnTask (fileSystem :: FILESYSTEM | e) RawError (Maybe Instant))

modified ::
  ∀  e
  .  FilePath
  -> Task (fileSystem :: FILESYSTEM | e) Error (Maybe Instant)
modified path =
  path
    |> normalize
    |> runFn3 _modified Task.ffiHelpers Instant.instant
    |> Task.fromEffFnTask "FileSystem.modified"
    |> lmap liftRawError


isOlderThan ::
  ∀  a e
  .  Duration a
  => a
  -> FilePath
  -> Task (fileSystem :: FILESYSTEM, now :: NOW | e) Error Boolean
isOlderThan duration path = do
  fileExists <- exists path
  if fileExists
    then do
      now <- lmap (UnknownRuntimeCrash >>> { details: _, path } >>> Error) <| Task.liftEff "Now.now" Now.now
      maybeModifiedTime <- modified path
      case maybeModifiedTime of
        Just modifiedTime ->
          let diff = DateTime.diff (Instant.toDateTime now) (Instant.toDateTime modifiedTime) :: a
          in pure <| Duration.fromDuration diff > Duration.fromDuration duration
        Nothing ->
          pure false
    else
      pure false


foreign import _remove ::
  ∀ e x
  . Fn2
      FfiHelpers
      FilePath
      (EffFnTask (fileSystem :: FILESYSTEM | e) x Unit)

remove ::
  ∀  e x
  .  FilePath
  -> Task (fileSystem :: FILESYSTEM | e) x Unit
remove path =
  path
    |> normalize
    |> runFn2 _remove Task.ffiHelpers
    |> Task.fromEffFnTask "FileSystem.remove"


-- FILEPATH STUFF


type FilePath = String


infixr 5 joinPath as </>


joinPath :: FilePath -> FilePath -> FilePath
joinPath "" p                                = p
joinPath p  ""                               = p
joinPath p p' | hasTrailing p && absolute p' = p <> drop 1 p'
joinPath p p' | hasTrailing p                = p <> p'
joinPath p p' | absolute p'                  = p <> p'
joinPath p p'                                = p <> "/" <> p'


infixr 5 extension as <.>


extension :: FilePath -> String -> FilePath
extension p ext =
  p <> "." <> ext


absolute :: FilePath -> Boolean
absolute p = (singleton <$> charAt 0 p) == Just "/"


hasTrailing :: FilePath -> Boolean
hasTrailing p = (singleton <$> charAt (length p - 1) p) == Just "/"


joinParts :: Array FilePath -> FilePath
joinParts ps = foldl (</>) "" $ nonEmpty ps


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
