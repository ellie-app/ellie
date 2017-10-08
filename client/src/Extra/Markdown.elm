module Extra.Markdown exposing (toString)

import Markdown.Block as Block exposing (Block(..), CodeBlock(..), ListType(..))
import Markdown.Inline as Inline exposing (Inline(..))


toString : String -> String
toString raw =
    raw
        |> Block.parse Nothing
        |> List.map blockToString
        |> String.join ""


blockToString : Block b i -> String
blockToString block =
    case block of
        BlankLine string ->
            ""

        ThematicBreak ->
            "<hr />"

        Heading _ level inlines ->
            let
                parsedInlines =
                    String.join " " (List.map inlineToString inlines)
            in
            case level of
                1 ->
                    "<h1>" ++ parsedInlines ++ "</h1>"

                2 ->
                    "<h2>" ++ parsedInlines ++ "</h2>"

                3 ->
                    "<h3>" ++ parsedInlines ++ "</h3>"

                4 ->
                    "<h4>" ++ parsedInlines ++ "</h4>"

                5 ->
                    "<h5>" ++ parsedInlines ++ "</h5>"

                _ ->
                    "<h6>" ++ parsedInlines ++ "</h6>"

        CodeBlock (Fenced _ model) codeStr ->
            case model.language of
                Just language ->
                    "<pre><code class=\"language-" ++ language ++ "\">" ++ codeStr ++ "</code></pre>"

                Nothing ->
                    "<pre><code>" ++ codeStr ++ "</code></pre>"

        CodeBlock Indented codeStr ->
            "<pre><code>" ++ codeStr ++ "</code></pre>"

        Paragraph _ inlines ->
            "<p>" ++ String.join " " (List.map inlineToString inlines) ++ "</p>"

        BlockQuote blocks ->
            "<blockquote>"
                ++ String.join " " (List.map blockToString blocks)
                ++ "</blockquote>"

        List model items ->
            let
                itemString =
                    items
                        |> List.concat
                        |> List.map (\item -> "<li>" ++ blockToString item ++ "</li>")
                        |> String.join ""
            in
            case model.type_ of
                Ordered 1 ->
                    "<ol>" ++ itemString ++ "</ol>"

                Ordered start ->
                    "<ol start=\"" ++ Basics.toString start ++ "\">" ++ itemString ++ "</ol>"

                Unordered ->
                    "<ul>" ++ itemString ++ "</ul>"

        PlainInlines inlines ->
            inlines
                |> List.map inlineToString
                |> String.join " "

        Block.Custom _ _ ->
            ""


inlineToString : Inline i -> String
inlineToString inline =
    case inline of
        Text string ->
            string

        HardLineBreak ->
            "<br />"

        CodeInline codeString ->
            "<code>" ++ codeString ++ "</code>"

        Link url title inlines ->
            "<a href=\""
                ++ url
                ++ "\""
                ++ (title |> Maybe.map ((++) " title=\"") |> Maybe.map (flip (++) "\"") |> Maybe.withDefault "")
                ++ ">"
                ++ String.join " " (List.map inlineToString inlines)
                ++ "</a>"

        Image url title inlines ->
            "<img alt=\""
                ++ Inline.extractText inlines
                ++ "\" src=\""
                ++ url
                ++ "\""
                ++ (title |> Maybe.map ((++) " title=\"") |> Maybe.map (flip (++) "\"") |> Maybe.withDefault "")
                ++ "/>"

        HtmlInline tag attrs inlines ->
            "<"
                ++ tag
                ++ " "
                ++ String.join " " (List.map attributeToString attrs)
                ++ ">"
                ++ String.join " " (List.map inlineToString inlines)
                ++ "</"
                ++ tag
                ++ ">"

        Emphasis length inlines ->
            case length of
                1 ->
                    "<em>"
                        ++ String.join " " (List.map inlineToString inlines)
                        ++ "</em>"

                2 ->
                    "<strong>"
                        ++ String.join " " (List.map inlineToString inlines)
                        ++ "</strong>"

                _ ->
                    if length - 2 > 0 then
                        "<strong>"
                            ++ inlineToString (Emphasis (length - 2) inlines)
                            ++ "</strong>"
                    else
                        "<em>"
                            ++ String.join " " (List.map inlineToString inlines)
                            ++ "</em>"

        Inline.Custom _ _ ->
            ""


attributeToString : ( String, Maybe String ) -> String
attributeToString ( name, maybeValue ) =
    name ++ "=\"" ++ Maybe.withDefault "" maybeValue ++ "\""
