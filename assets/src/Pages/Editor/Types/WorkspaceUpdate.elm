module Pages.Editor.Types.WorkspaceUpdate exposing (..)

import Elm.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)


type WorkspaceUpdate
    = Attached (List Package)
    | CompileCompleted (Maybe Error)
    | Ping
