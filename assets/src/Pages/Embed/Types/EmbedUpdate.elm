module Pages.Embed.Types.EmbedUpdate exposing (..)

import Elm.Error as Error exposing (Error)


type EmbedUpdate
    = Connected
    | Compiled (Maybe Error)
    | Failed String
    | Disconnected
