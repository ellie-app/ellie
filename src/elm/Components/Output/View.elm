module Components.Output.View
    exposing
        ( success
        , waiting
        , compiling
        , errors
        )

import Html exposing (Html, div, iframe, text)
import Html.Attributes exposing (src)
import Types.CompileError as CompileError exposing (CompileError)
import Components.Output.Classes exposing (Classes(..), class)
import Shared.Utils as Utils
import Shared.Constants as Constants


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
            [ Utils.innerHtml <| Utils.replaceAll compileError.overview
            , class [ ErrorItemOverview ]
            ]
            []
        , div
            [ Utils.innerHtml <| Utils.replaceAll compileError.details
            , class [ ErrorItemDetails ]
            ]
            []
        ]


errors : List CompileError -> Html msg
errors compileErrors =
    div [ class [ ErrorsContainer ] ]
        (List.map errorSection compileErrors)


success : String -> Html msg
success sessionId =
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
