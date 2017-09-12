module Data.Ellie.Notification
    exposing
        ( Action(..)
        , Level(..)
        , Notification
        , hash
        )

import Time exposing (Time)


type Action
    = ClearElmStuff


type Level
    = Error
    | Warning
    | Success
    | Info


type alias Notification =
    { level : Level
    , message : String
    , title : String
    , action : Maybe Action
    , timestamp : Time
    }


hash : Notification -> String
hash notification =
    toString notification
