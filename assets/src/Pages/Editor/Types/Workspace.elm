module Pages.Editor.Types.Message exposing (Event(..), Incoming(..), Outgoing(..))

import Elm.Compiler.Error as Compiler
import Elm.Package as Package exposing (Package)


type Outgoing
    = FormatElmCode String
    | Compile String String (List Package)


type Incoming
    = CompileFinished (List Compiler.Error)
    | Attached (List Package)
    | ElmCodeFormatted String


type Event
    = Message Incoming
    | Disconnected
    | Connected
    | Error String
