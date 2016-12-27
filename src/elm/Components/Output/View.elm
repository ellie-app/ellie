module Components.Output.View
    exposing
        ( loading
        , success
        , waiting
        , compiling
        , errors
        )

import Regex exposing (Regex)
import Json.Encode as Encode
import Html exposing (Html, div, iframe, text)
import Html.Attributes exposing (src)
import Types.CompileError as CompileError exposing (CompileError)
import Components.Output.Classes exposing (Classes(..), class)
import Shared.Utils as Utils
import Shared.Constants as Constants


innerHtml : String -> Html.Attribute msg
innerHtml string =
    Html.Attributes.property "innerHTML" <| Encode.string string


loadingSection : Html msg
loadingSection =
    div [ class [ LoadingSection ] ]
        [ div [ class [ LoadingFullBox ] ] []
        , div [ class [ LoadingSplitContainer ] ]
            [ div [ class [ LoadingSplitLeft ] ]
                [ div [ class [ LoadingFullBox ] ] []
                , div [ class [ LoadingFullBox ] ] []
                ]
            , div [ class [ LoadingSplitRight ] ]
                [ div [ class [ LoadingCircle ] ] []
                ]
            ]
        ]


replaceBackticks : String -> String
replaceBackticks string =
    Regex.replace
        Regex.All
        (Regex.regex "`([^`]+)`")
        (\match ->
            match.submatches
                |> List.head
                |> Maybe.andThen identity
                |> Maybe.map (\submatch -> "<code>" ++ submatch ++ "</code>")
                |> Maybe.withDefault match.match
        )
        string


replaceNewlines : String -> String
replaceNewlines string =
    Regex.replace
        Regex.All
        (Regex.regex "\n")
        (\_ -> "<br />")
        string


replaceAll : String -> String
replaceAll string =
    string
        |> replaceBackticks
        |> replaceNewlines


errorSection : CompileError -> Html msg
errorSection compileError =
    div [ class [ ErrorItem ] ]
        [ div [ class [ ErrorItemHeader ] ]
            [ div [ class [ ErrorItemName ] ]
                [ text compileError.tag ]
            , div [ class [ ErrorItemLocation ] ]
                [ text <| "line " ++ toString compileError.region.start.line ++ " column " ++ toString compileError.region.start.column ]
            ]
        , div
            [ innerHtml <| replaceAll compileError.overview
            , class [ ErrorItemOverview ]
            ]
            []
        , div
            [ innerHtml <| replaceAll compileError.details
            , class [ ErrorItemDetails ]
            ]
            []
        ]


errors : List CompileError -> Html msg
errors compileErrors =
    div [ class [ ErrorsContainer ] ]
        (List.map errorSection compileErrors)


loading : Html msg
loading =
    div [ class [ Loading ] ]
        [ loadingSection
        , loadingSection
        , div [ class [ LoadingFullBox ] ] []
        , div [ class [ LoadingShimmer ] ] []
        ]


success : String -> String -> Html msg
success sessionId htmlCode =
    iframe
        [ src <| (Constants.apiBase ++ "/sessions/" ++ sessionId ++ "/iframe")
        , class [ Iframe ]
        ]
        []


overlayDisplay : String -> String -> Html msg
overlayDisplay title subtitle =
    div [ class [ Overlay ] ]
        [ div [ class [ OverlayTitle ] ]
            [ text title ]
        , div [ class [ OverlaySubtitle ] ]
            [ text subtitle ]
        ]


compiling : Html msg
compiling =
    overlayDisplay "Compiling!" "This shouldn't take too long."


waiting : Html msg
waiting =
    overlayDisplay "Ready!" "Run the compiler to see your program."
