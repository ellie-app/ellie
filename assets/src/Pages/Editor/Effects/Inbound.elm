port module Pages.Editor.Effects.Inbound
    exposing
        ( Inbound(..)
        , batch
        , map
        , none
        )

import Data.Jwt exposing (Jwt)
import Elm.Compiler.Error as CompilerError
import Elm.Package as Package exposing (Package)


type Inbound msg
    = CompileFinished Jwt (List CompilerError.Error -> msg)
    | OutputThrewException (String -> msg)
    | WorkspaceAttached Jwt (List Package -> msg)
    | KeepWorkspaceOpen Jwt
    | EscapePressed msg
    | WorkspaceDetached Jwt msg
    | Batch (List (Inbound msg))
    | None


batch : List (Inbound msg) -> Inbound msg
batch =
    Batch


none : Inbound msg
none =
    None


map : (a -> b) -> Inbound a -> Inbound b
map f inbound =
    case inbound of
        WorkspaceDetached token next ->
            WorkspaceDetached token (f next)

        EscapePressed next ->
            EscapePressed (f next)

        CompileFinished token callback ->
            CompileFinished token (callback >> f)

        OutputThrewException callback ->
            OutputThrewException (callback >> f)

        WorkspaceAttached token callback ->
            WorkspaceAttached token (callback >> f)

        KeepWorkspaceOpen token ->
            KeepWorkspaceOpen token

        Batch inbounds ->
            Batch <| List.map (map f) inbounds

        None ->
            None
