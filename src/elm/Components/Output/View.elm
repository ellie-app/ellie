module Components.Output.View
    exposing
        ( success
        , initial
        , compiling
        , errors
        , failure
        , loadingCompiler
        )

import Html exposing (Html, div, iframe, text)
import Html.Attributes exposing (src, id)
import Types.CompileError as CompileError exposing (CompileError)
import Types.CompileStage as CompileStage exposing (CompileStage)
import Components.Output.Classes exposing (Classes(..), class)
import Shared.Utils as Utils
import Shared.Constants as Constants


overlayDisplay : String -> String -> Html msg
overlayDisplay title subtitle =
    div [ class [ Overlay ] ]
        [ div [ class [ OverlayTitle ] ]
            [ text title ]
        , div [ class [ OverlaySubtitle ] ]
            [ text subtitle ]
        ]


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
success iframeUrl =
    iframe
        [ src <| iframeUrl
        , class [ Iframe ]
        , id "results_iframe"
        ]
        []


failure : Html msg
failure =
    overlayDisplay "Oh no!" "Something went wrong when compiling."


compiling : Int -> Int -> Html msg
compiling total complete =
    overlayDisplay
        "Compiling..."
        ("Compiled " ++ toString complete ++ " of " ++ toString total ++ " modules")


loadingCompiler : Float -> Html msg
loadingCompiler percentage =
    overlayDisplay
        "Loading Compiler..."
        (toString (round (percentage * 100)) ++ "% loaded")


initial : Html msg
initial =
    overlayDisplay "Ready!" "Run the compiler to see your program."
