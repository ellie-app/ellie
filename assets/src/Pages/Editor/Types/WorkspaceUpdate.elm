module Pages.Editor.Types.WorkspaceUpdate exposing (WorkspaceUpdate(..))

import Data.Jwt as Jwt exposing (Jwt)
import Elm.Error as Error exposing (Error)
import Elm.Package as Package exposing (Package)


type WorkspaceUpdate
    = Connected
    | Attached (List Package)
    | CompileCompleted (Maybe Error)
    | Disconnected
