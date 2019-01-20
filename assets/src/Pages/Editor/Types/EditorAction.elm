module Pages.Editor.Types.EditorAction exposing (EditorAction(..))


type EditorAction
    = OpenPackages
    | OpenSettings
    | OpenDebugger
    | OpenOutput
    | Recompile
    | ReloadOutput
    | OpenLogs
    | RecoverCrash
    | Save
