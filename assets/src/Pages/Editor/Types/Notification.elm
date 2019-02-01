module Pages.Editor.Types.Notification exposing
    ( Action(..)
    , Notification
    , Severity(..)
    , eq
    )

import Pages.Editor.Types.EditorAction as EditorAction exposing (EditorAction)


type Action
    = CopyLink String
    | GoToLink String
    | PerformAction String EditorAction


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
