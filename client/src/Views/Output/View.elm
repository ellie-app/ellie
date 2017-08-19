module Views.Output.View
    exposing
        ( compiling
        , errors
        , failure
        , generating
        , initial
        , installing
        , loadingCompiler
        , planning
        , success
        )

import Data.Elm.Compiler.Error as CompilerError
import Html exposing (Html, div, iframe, p, text)
import Html.Attributes exposing (id, src)
import Shared.Utils as Utils
import Views.Output.Classes exposing (Classes(..), class)
import Views.ProgressBar.View as ProgressBar


overlayDisplay : String -> List (Html msg) -> Html msg
overlayDisplay title subtitleContent =
    div [ class [ Overlay ] ]
        [ div [ class [ OverlayContent ] ]
            [ div [ class [ OverlayTitle ] ]
                [ text title ]
            , div [ class [ OverlaySubtitle ] ]
                subtitleContent
            ]
        ]


errorSection : CompilerError.Error -> Html msg
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


errors : List CompilerError.Error -> Html msg
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
    overlayDisplay "Oh no!" [ text "Something went wrong when compiling." ]


installing : Html msg
installing =
    overlayDisplay
        "Installing Packages"
        []


generating : Html msg
generating =
    overlayDisplay
        "Compiling..."
        [ ProgressBar.view
            { percentage = 1
            , label = Just "Generating output"
            }
        ]


planning : Html msg
planning =
    overlayDisplay
        "Compiling..."
        [ ProgressBar.view
            { percentage = 0.05
            , label = Just "Planning your build"
            }
        ]


compiling : Int -> Int -> Html msg
compiling total complete =
    let
        progressBar =
            ProgressBar.view
                { percentage = 0.05 + (toFloat complete / toFloat total) * 0.9
                , label = Just <| toString complete ++ " / " ++ toString total ++ " Modules Compiled"
                }

        subtitleContent =
            case total >= 5 of
                True ->
                    [ progressBar
                    , p [ class [ ManyModulesWarning ] ]
                        [ text "We're compiling some modules for the first time. It may take a while but it'll be fast next time!" ]
                    ]

                False ->
                    [ progressBar ]
    in
    overlayDisplay
        "Compiling..."
        subtitleContent


loadingCompiler : Float -> Html msg
loadingCompiler percentage =
    overlayDisplay
        "Loading Compiler..."
        [ ProgressBar.view
            { percentage = percentage
            , label = Nothing
            }
        ]


initial : Html msg
initial =
    overlayDisplay "Ready!" [ text "Run the compiler to see your program." ]
