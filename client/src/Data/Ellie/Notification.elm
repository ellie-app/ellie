module Data.Ellie.Notification
    exposing
        ( Level(..)
        , Notification
        , eq
        , hash
        )

import Time exposing (Time)


type Level
    = Error
    | Warning
    | Success
    | Info


type alias Notification =
    { level : Level
    , message : String
    , title : String
    , timestamp : Time
    }


hash : Notification -> String
hash notification =
    toString notification


eq : Notification -> Notification -> Bool
eq left right =
    (left.level == right.level)
        && (left.message == right.message)
        && (left.title == right.title)
