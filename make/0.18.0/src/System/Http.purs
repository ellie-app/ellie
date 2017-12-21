module System.Http
  ( HTTP
  , Request
  , Response
  , Error(..)
  , Expect
  , expectStringResponse
  , expectString
  , expectJson
  , expectBlob
  , NoContent
  , expectNoContent
  , get
  , put
  , post
  , patch
  , delete
  , withHeader
  , withCredentials
  , withExpect
  , withTimeout
  , withProgress
  , send
  ) where

import Ellie.Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe as Eff
import Control.Monad.Except (except, runExcept)
import Control.Monad.Task (kind Effect, EffFnTask, Task)
import Control.Monad.Task as Task
import Data.Array (intercalate, (:))
import Data.Bifunctor (lmap)
import Data.Blob (Blob)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign (renderForeignError) as Foreign
import Data.Foreign.Class (class Foreignable)
import Data.Foreign.Class (get) as Foreign
import Data.Function.Uncurried (Fn2, Fn3, Fn4, mkFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.Url (Url)


foreign import data HTTP :: Effect


newtype Request a =
  Request
    { url :: Url
    , method :: String
    , headers :: Array { key :: String, value :: String }
    , withCredentials :: Boolean
    , timeout :: Maybe Int
    , expect :: Expect a
    , progress :: Maybe Handler
    }


type Response a =
  { url :: String
  , headers :: StrMap String
  , body :: a
  , code :: Int
  , message :: String
  }


data Error
  = BadUrl String
  | Timeout
  | NetworkError
  | BadStatus (Response String)
  | BadPayload String (Response String)


instance showError :: Show Error where
  show (BadUrl url) = "URL `" <> url <> "` is not valid"
  show Timeout = "Request timed out"
  show NetworkError = "Network failed"
  show (BadStatus response) = "Response had a failed status code: " <> show response.code <> ", " <> show response.message
  show (BadPayload message _) = "Couldn't understand response body: " <> show message



type FfiErrors =
  { networkError :: Error
  , timeout :: Error
  , badUrl :: String -> Error
  , badStatus :: Response String -> Error
  , badPayload :: Fn2 String (Response String) Error
  }

errors :: FfiErrors
errors =
  { networkError: NetworkError
  , timeout: Timeout
  , badUrl: BadUrl
  , badStatus: BadStatus
  , badPayload: mkFn2 BadPayload
  }


foreign import data Expect :: Type -> Type


foreign import _expectStringResponse :: ∀ a. (Response String -> Either String a) -> Expect a
expectStringResponse :: ∀ a. (Response String -> Either String a) -> Expect a
expectStringResponse = _expectStringResponse


expectString :: Expect String
expectString =
  expectStringResponse (_.body >>> Right)


expectJson :: ∀ a. Foreignable a => Expect a
expectJson =
  let
    parser :: Response String -> Either String a
    parser response = do
      foreignData <-
        parseJson response.body

      parsed <-
        foreignData
          |> Foreign.get
          |> runExcept
          |> lmap (map Foreign.renderForeignError >>> intercalate "\n")
      
      pure parsed
  in
    expectStringResponse parser


foreign import _expectBlobResponse :: ∀ a. (Response Blob -> Either String a) -> Expect a
expectBlobResponse :: ∀ a. (Response Blob -> Either String a) -> Expect a
expectBlobResponse = _expectBlobResponse


expectBlob :: Expect Blob
expectBlob =
  expectBlobResponse (_.body >>> Right)


data NoContent = NoContent


foreign import _expectUntypedResponse :: ∀ a. (Response Unit -> Either String a) -> Expect a
expectNoContent :: Expect NoContent
expectNoContent =
  _expectUntypedResponse (const $ Right NoContent)


requestWithMethod :: String -> Url -> Request NoContent
requestWithMethod method url =
  Request
    { url: url
    , method: method
    , headers: []
    , withCredentials: false
    , timeout: Nothing
    , expect: expectNoContent
    , progress: Nothing
    }


get :: Url -> Request NoContent
get = requestWithMethod "GET"


put :: Url -> Request NoContent
put = requestWithMethod "PUT"


post :: Url -> Request NoContent
post = requestWithMethod "POST"


patch :: Url -> Request NoContent
patch = requestWithMethod "PATCH"


delete :: Url -> Request NoContent
delete = requestWithMethod "DELETE"


type Handler =
  { total :: Int, loaded :: Int } -> Unit


withProgress :: ∀ a e. ({ total :: Int, loaded :: Int } -> Eff e Unit) -> Request a -> Request a
withProgress handler (Request request) =
  Request $
    request
      { progress = Just (handler >>> Eff.unsafePerformEff)
      }


withHeader :: ∀ a. String -> String -> Request a -> Request a
withHeader key value (Request request) =
  Request $ request { headers = { key, value } : request.headers }


withCredentials :: ∀ a. Boolean -> Request a -> Request a
withCredentials include (Request request) =
  Request $ request { withCredentials = include }


withExpect :: ∀ a b. Expect b -> Request a -> Request b
withExpect expect (Request request) =
  Request $ request { expect = expect }


withTimeout :: ∀ a. Int -> Request a -> Request a
withTimeout timeout (Request request) =
  Request $ request { timeout = Just timeout }


foreign import _send ::
  ∀ e x a b.
    Fn4
      Task.FfiHelpers
      FfiErrors
      (Url -> String)
      (Request a)
      (EffFnTask e x a)


send :: ∀ a e. Request a -> Task (http :: HTTP | e) Error a
send request =
  Task.fromEffFnTask "Http.send" <|
    runFn4
      _send
      Task.ffiHelpers
      errors
      show
      request


foreign import _parseJson ::
  Fn3
    (Foreign -> Either String Foreign)
    (String -> Either String Foreign)
    String
    (Either String Foreign)


parseJson :: String -> Either String Foreign
parseJson =
  runFn3 _parseJson Right Left