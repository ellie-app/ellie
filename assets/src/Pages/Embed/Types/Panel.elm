module Pages.Embed.Types.Panel exposing (..)


type Panel
    = Elm
    | Html
    | Output
    | Debugger


fromString : String -> Panel
fromString input =
    case input of
        "html" ->
            Html

        "output" ->
            Output

        "debugger" ->
            Debugger

        _ ->
            Elm


toString : Panel -> String
toString panel =
    case panel of
        Html ->
            "html"

        Output ->
            "output"

        Debugger ->
            "debugger"

        Elm ->
            "elm"
