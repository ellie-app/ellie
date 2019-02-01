module Pages.Embed.Types.Panel exposing (Panel(..), eq, fromString, toString)


type Panel
    = Elm
    | Html
    | Output


eq : Panel -> Panel -> Bool
eq left right =
    case ( left, right ) of
        ( Elm, Elm ) ->
            True

        ( Html, Html ) ->
            True

        ( Output, Output ) ->
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

        _ ->
            Elm


toString : Panel -> String
toString panel =
    case panel of
        Html ->
            "html"

        Output ->
            "output"

        Elm ->
            "elm"
