module Data.Json
  ( Json
  , Error(..)
  , parse
  , decodeBoolean
  , decodeInt
  , decodeNumber
  , decodeString
  , decodeNull
  , decodeArray
  , decodeKeyValues
  , decodeAtIndex
  , decodeAtField
  , decodeAtPath
  , decodeForeign
  , encodeInt
  , encodeNumber
  , encodeString
  , encodeBoolean
  , encodeNull
  , encodeArray
  , encodeObject
  , stringify
  ) where

import Prelude

import Control.Monad.Except as Except
import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Char.Unicode as Char
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Foreign (Foreign, F)
import Data.Foreign (ForeignError(..), fail, isArray, isNull, readArray, readBoolean, readInt, readNumber, readString, renderForeignError, toForeign) as Foreign
import Data.Foreign.Class (decode) as Foreign
import Data.Foreign.Generic (encodeJSON, decodeJSON) as Foreign
import Data.Foreign.Index (index) as Foreign
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String (Pattern(..))
import Data.String (joinWith, split, toCharArray, uncons) as String
import Data.String.Class (class ToString)
import Data.String.Class (toString) as String
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))

foreign import _null ∷ Json
foreign import _mergeObjects ∷ Json → Json → Json


newtype Json =
  Json Foreign


data Error
  = Failure String Json
  | Index Int Error
  | Field String Error
  | OneOf (Array Error)


instance toStringError ∷ ToString Error where
  toString error =
    toStringHelp error []
    where
      toStringHelp ∷ Error → Array String → String
      toStringHelp (Field f err) context =
        toStringHelp err (fieldName : context)
        where
          isSimple =
            case String.uncons f of
              Nothing → false
              Just { head, tail } → Char.isAlpha head && Foldable.all Char.isAlphaNum (String.toCharArray tail)
          fieldName =
            if isSimple then
              "." <> f
            else
              "['" <> f <> "']"

      toStringHelp (Index i err) context =
        toStringHelp err (("[" <> String.toString i <> "]") : context)

      toStringHelp (OneOf errors) context =
        case errors of
          [] →
            "Ran into a Json.Decode.oneOf with no possibilities" <>
              case context of
                [] → "!"
                _ → " at json" <> String.joinWith "" (Array.reverse context)
          [err] →
            toStringHelp err context
          _ →
            let
              starter =
                case context of
                  [] → "Json.Decode.oneOf"
                  _ → "The Json.Decode.oneOf at json" <> String.joinWith "" (Array.reverse context)

              introduction =
                starter <> " failed in the following " <> String.toString (Array.length errors) <> " ways:"
            in
              String.joinWith "\n\n" (introduction : Array.mapWithIndex errorOneOf errors)

      toStringHelp (Failure msg json) context =
        let
          introduction =
            case context of
              [] → "Problem with the given value:\n\n"
              _ → "Problem with the value at json" <> String.joinWith "" (Array.reverse context) <> ":\n\n    "
        in
          introduction <> indent (stringify json) <> "\n\n" <> msg

      errorOneOf ∷ Int → Error → String
      errorOneOf i error =
        "\n\n(" <> String.toString (i + 1) <> ") " <> indent (String.toString error)

      indent ∷ String → String
      indent str =
        String.joinWith "\n    " (String.split (Pattern "\n") str)


parse ∷ String → Either Error Json
parse input =
  input
    # Foreign.decodeJSON
    # Except.runExcept
    # map Json
    # lmap \errors →
        errors
          # NonEmpty.head
          # Foreign.renderForeignError
          # ("This is not valid JSON! " <> _)
          # (\message → Failure message (Json (Foreign.toForeign input)))



decodeBoolean ∷ Json → Either Error Boolean
decodeBoolean json@(Json value) =
  Foreign.readBoolean value
    # Except.runExcept
    # lmap (\_ → Failure "Expecting a Boolean" json)


decodeInt ∷ Json → Either Error Int
decodeInt json@(Json value) =
  Foreign.readInt value
    # Except.runExcept
    # lmap (\_ → Failure "Expecting an Int" json)


decodeNumber ∷ Json → Either Error Number
decodeNumber json@(Json value) =
  Foreign.readNumber value
    # Except.runExcept
    # lmap (\_ → Failure "Expecting a Number" json)


decodeString ∷ Json → Either Error String
decodeString json@(Json value) =
  Foreign.readString value
    # Except.runExcept
    # lmap (\_ → Failure "Expecting a String" json)


decodeNull ∷ ∀ a. a → Json → Either Error a
decodeNull to json@(Json value) =
  if Foreign.isNull value then
    Right to
  else
    Left $ Failure "Expecting null" json


decodeArray ∷ ∀ a. (Json → Either Error a) → Json → Either Error (Array a)
decodeArray decoder json@(Json value) = do
  array ←
    Foreign.readArray value
      # Except.runExcept
      # lmap (\_ → Failure "Expecting an Array" json)
  traverseWithIndex decodeArrayHelp array
  where
    decodeArrayHelp i v =
      lmap (Index i) (decoder (Json v))


decodeKeyValues ∷ ∀ a. (Json → Either Error a) → Json → Either Error (Array { key ∷ String, value ∷ a})
decodeKeyValues decoder json@(Json value) = do
  (strMap ∷ StrMap Foreign) ←
    Foreign.decode value
      # Except.runExcept
      # lmap (\_ → Failure "Expecting an Object" json)
  strMap
    # StrMap.toUnfoldable
    # traverse (\(Tuple key value) → decoder (Json value) <#> { key, value: _ })


decodeAtIndex ∷ ∀ a. Int → Json → (Json → Either Error a) → Either Error a
decodeAtIndex index json@(Json value) decoder =
  if not (Foreign.isArray value) then
    Left $ Failure "Expecting an Array" json
  else do
    array ←
      Foreign.readArray value
        # Except.runExcept
        # lmap (\_ → Failure "Expecting an Array" json)
    atIndex ←
      Either.note
        (Failure ("Expecting a longer Array. Need index " <> show index <> " but only see " <> show (Array.length array) <> " entries") json)
        (Array.index array index)
    atIndex
      # Json
      # decoder
      # lmap (Index index)


decodeAtField ∷ ∀ a. String → Json → (Json → Either Error a) → Either Error a
decodeAtField field json@(Json value) decoder = do
  atField ←
    Foreign.index value field 
      # Except.runExcept
      # lmap (\_ → Failure ("Expecting an Object with a field named `" <> field <> "`") json)
  atField
    # Json
    # decoder
    # lmap (Field field)


decodeAtPath ∷ ∀ a. Array String → Json → (Json → Either Error a) → Either Error a
decodeAtPath path json decoder =
  case Array.uncons path of
    Nothing → decoder json
    Just { head, tail } →
      case decodeAtField head json Right of
        Left error → Left error
        Right atField →
          case decodeAtPath tail atField decoder of
            Left error → Left $ Field head error
            Right value → Right value
      

decodeForeign ∷ ∀ a. Foreign → (Json → Either Error a) → F a
decodeForeign value decoder =
  case decoder (Json value) of
    Left e → Foreign.fail $ Foreign.JSONError (String.toString e)
    Right v → pure v


encode' ∷ ∀ a. a → Json
encode' a =
  Json $ Foreign.toForeign a


encodeInt ∷ Int → Json
encodeInt = encode'


encodeNumber ∷ Number → Json
encodeNumber = encode'


encodeString ∷ String → Json
encodeString = encode'


encodeBoolean ∷ Boolean → Json
encodeBoolean = encode'


encodeNull ∷ Json
encodeNull = encode' _null


encodeArray ∷ ∀ a. (a → Json) → Array a → Json
encodeArray encoder array =
  encode' $ map encoder array


encodeObject ∷ ∀ a. Array { key ∷ String, value ∷ Json } → Json
encodeObject kvps =
  kvps
    # map (\{ key, value: Json value } → Tuple key value)
    # StrMap.fromFoldable
    # encode'


mergeObjects ∷ Json → Json → Json
mergeObjects = _mergeObjects


stringify ∷ Json → String
stringify (Json value) =
  Foreign.encodeJSON value
