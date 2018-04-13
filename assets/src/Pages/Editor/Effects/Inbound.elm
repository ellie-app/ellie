port module Pages.Editor.Effects.Inbound
    exposing
        ( Inbound(..)
        , batch
        , flatten
        , map
        , none
        )

import Data.Jwt exposing (Jwt)
import Elm.Error as ElmError
import Elm.Package as Package exposing (Package)
import Pages.Editor.Types.WorkspaceUpdate as WorkspaceUpdate exposing (WorkspaceUpdate)


type Inbound msg
    = OutputThrewException (String -> msg)
    | WorkspaceUpdates Jwt (WorkspaceUpdate -> msg)
    | EscapePressed msg
    | Batch (List (Inbound msg))
    | None


batch : List (Inbound msg) -> Inbound msg
batch =
    Batch


none : Inbound msg
none =
    None


flatten : Inbound msg -> List (Inbound msg)
flatten inbound =
    case inbound of
        None ->
            []

        Batch others ->
            List.concatMap flatten others

        _ ->
            [ inbound ]


map : (a -> b) -> Inbound a -> Inbound b
map f inbound =
    case inbound of
        WorkspaceUpdates token callback ->
            WorkspaceUpdates token (callback >> f)

        EscapePressed next ->
            EscapePressed (f next)

        OutputThrewException callback ->
            OutputThrewException (callback >> f)

        Batch inbounds ->
            Batch <| List.map (map f) inbounds

        None ->
            None
