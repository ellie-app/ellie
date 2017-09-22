module Views.Output
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
import Views.Output.Styles as Styles
import Views.ProgressBar as ProgressBar


overlayDisplay : String -> List (Html msg) -> Html msg
overlayDisplay title subtitleContent =
    div [ Styles.overlay ]
        [ div [ Styles.overlayContent ]
            [ div [ Styles.overlayTitle ]
                [ text title ]
            , div [ Styles.overlaySubtitle ]
                subtitleContent
            ]
        ]


errorSection : CompilerError.Error -> Html msg
errorSection compileError =
    div [ Styles.errorItem ]
        [ div [ Styles.errorItemHeader ]
            [ div [ Styles.errorItemName ]
                [ text compileError.tag ]
            , div [ Styles.errorItemLocation ]
                [ text <| "line " ++ toString compileError.region.start.line ++ " column " ++ toString compileError.region.start.column ]
            ]
        , div
            [ Utils.innerHtml <| Utils.replaceAll compileError.overview
            , Styles.errorItemOverview
            ]
            []
        , div
            [ Utils.innerHtml <| Utils.replaceAll compileError.details
            , Styles.errorItemDetails
            ]
            []
        ]


errors : List CompilerError.Error -> Html msg
errors compileErrors =
    div [ Styles.errorsContainer ]
        (List.map errorSection compileErrors)


success : String -> Html msg
success iframeUrl =
    iframe
        [ src <| iframeUrl
        , Styles.iframe
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
                , label =
                    if total == 1 then
                        Just "Compiling Elm code"
                    else
                        Just <| toString complete ++ " / " ++ toString total ++ " Modules Compiled"
                }

        subtitleContent =
            case total >= 5 of
                True ->
                    [ progressBar
                    , p [ Styles.manyModulesWarning ]
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
