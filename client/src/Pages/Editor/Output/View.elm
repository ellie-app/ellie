module Pages.Editor.Output.View exposing (..)

import Data.Ellie.CompileStage as CompileStage exposing (CompileStage(..))
import Data.Elm.Compiler.Error as Error exposing (Error)
import Ellie.Ui.Button as Button
import Ellie.Ui.CompileError as CompileError
import Ellie.Ui.ProgressBar as ProgressBar
import Extra.Html as Html
import Html exposing (Html, div, iframe, text)
import Html.Attributes exposing (id, src)
import Pages.Editor.Output.Styles as Styles


type alias Config msg =
    { stage : CompileStage
    , onClearElmStuff : msg
    }


viewReady : List (Html msg)
viewReady =
    [ div [ Styles.details ]
        [ div [ Styles.detailsTitle ] [ text "Ready" ]
        , div [ Styles.detailsSubMessage ] [ text "Run the compiler to use your program." ]
        ]
    ]


viewInstalling : List (Html msg)
viewInstalling =
    [ div [ Styles.details ]
        [ div [ Styles.detailsTitle ] [ text "Installing Packages" ]
        , div [ Styles.progressBarContainer ] [ ProgressBar.view ProgressBar.Indeterminate ]
        ]
    ]


viewPlanning : List (Html msg)
viewPlanning =
    [ div [ Styles.details ]
        [ div [ Styles.detailsTitle ] [ text "Compiling" ]
        , div [ Styles.detailsSubMessage ] [ text "Planning build" ]
        , div [ Styles.progressBarContainer ] [ ProgressBar.view <| ProgressBar.Percentage 0.05 ]
        ]
    ]


viewCompiling : Int -> Int -> List (Html msg)
viewCompiling complete total =
    let
        progressBar =
            div [ Styles.progressBarContainer ]
                [ ProgressBar.view <|
                    ProgressBar.Percentage
                        (0.05 + (toFloat complete / toFloat total) * 0.9)
                ]

        label =
            if total == 1 then
                div [ Styles.detailsSubMessage ] [ text "Compiling Elm code" ]
            else
                div [ Styles.detailsSubMessage ]
                    [ text <| toString complete ++ " / " ++ toString total ++ " Modules Compiled" ]

        notice =
            Html.viewIf (total >= 4) <|
                div [ Styles.detailsExtraNotice ]
                    [ text "We're compiling some modules for the first time. It may take a while but it'll be fast next time!" ]
    in
    [ div [ Styles.details ]
        [ div [ Styles.detailsTitle ] [ text "Compiling" ]
        , label
        , progressBar
        , notice
        ]
    ]


viewGenerating : List (Html msg)
viewGenerating =
    [ div [ Styles.details ]
        [ div [ Styles.detailsTitle ] [ text "Compiling" ]
        , div [ Styles.detailsSubMessage ] [ text "Generating Output" ]
        , div [ Styles.progressBarContainer ]
            [ ProgressBar.view <| ProgressBar.Percentage 1
            ]
        ]
    ]


viewSuccess : String -> List (Html msg)
viewSuccess iframeUrl =
    [ iframe
        [ src <| iframeUrl
        , id "results_iframe"
        , Styles.iframe
        ]
        []
    ]


viewErrors : List Error -> List (Html msg)
viewErrors errors =
    List.map
        (CompileError.view >> List.singleton >> div [])
        errors


viewFailure : Config msg -> String -> List (Html msg)
viewFailure config message =
    [ div []
        [ div [] [ text "Compilation Failed" ]
        , div [] [ text message ]
        ]
    , div []
        [ div [] [ text "Hint: sometimes clearing the compiler cache (elm-stuff) can help resolve compilation issues." ]
        , div []
            [ Button.view
                { disabled = False
                , label = "Clear elm-stuff"
                , icon = Nothing
                , style = Button.Primary
                , size = Button.Medium
                , action = Button.click config.onClearElmStuff
                , attributes = []
                }
            ]
        ]
    ]


view : Config msg -> Html msg
view config =
    div [ Styles.container ] <|
        case config.stage of
            Initial ->
                viewReady

            LoadingCompiler percentage ->
                [ div []
                    [ div [] [ text "Loading Compiler" ]
                    , div []
                        [ ProgressBar.view <| ProgressBar.Percentage percentage
                        ]
                    ]
                ]

            InstallingPackages ->
                viewInstalling

            PlanningBuild ->
                viewPlanning

            Compiling { total, complete } ->
                viewCompiling complete total

            GeneratingCode ->
                viewGenerating

            Success url ->
                viewSuccess url

            FinishedWithErrors errors ->
                viewErrors errors

            CompileStage.Failed message ->
                viewFailure config message
