module Pages.Editor.Types.Notification
    exposing
        ( Action(..)
        , Notification
        , Severity(..)
        , eq
        , fromException
        )

import Data.Url as Url exposing (Url)
import Pages.Editor.Effects.Exception exposing (Exception(..))


type Action
    = CopyLink Url
    | GoToLink String


type Severity
    = Info
    | Warning
    | Success
    | Failure


type alias Notification =
    { severity : Severity
    , message : String
    , title : String
    , actions : List Action
    }


eq : Notification -> Notification -> Bool
eq left right =
    left == right


fromException : Exception -> Notification
fromException exception =
    case exception of
        PackageServerUnavailable ->
            { severity = Failure
            , title = "elm-package is down"
            , message = "The server at package.elm-lang.org is unavailable! Some features of Ellie depend on it so you may have trouble working on your project."
            , actions = []
            }

        ClientNetworkError ->
            { severity = Failure
            , title = "Network error"
            , message = "The browser was unable to make a request to the server. Are you connected to the internet?"
            , actions = []
            }

        ClientDecoderFailure message ->
            { severity = Failure
            , title = "Broken data decoder"
            , message = "A JSON decoder for one of Ellie's data types is broken. This is a bug! It has been automatically reported to the maintainers, but things might be broken until the issue is fixed."
            , actions = []
            }

        Unknown message ->
            { severity = Failure
            , title = "Unhandled error"
            , message = "Something has gone wrong and we don't know what it was. It has been automatically reported to the maintainers, and you can see the error for yourself below:\n\n" ++ message
            , actions = []
            }
