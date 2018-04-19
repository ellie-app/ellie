module Pages.Embed.Types.Panel exposing (..)


type Panel
    = Elm
    | Html
    | Output
    | Debugger


eq : Panel -> Panel -> Bool
eq left right =
    case ( left, right ) of
        ( Elm, Elm ) ->
            True

        ( Html, Html ) ->
            True

        ( Output, Output ) ->
            True

        ( Debugger, Debugger ) ->
            True

        _ ->
            False


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
