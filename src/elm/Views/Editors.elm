module Views.Editors
    exposing
        ( elm
        , html
        , loading
        )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style, value)
import Api exposing (CompileError)
import Views.CodeMirror as CodeMirror
import Views.Classes as Classes exposing (Classes(..), class)


singleton : a -> List a
singleton a =
    [ a ]


loadingLine : Float -> Html msg
loadingLine width =
    div
        [ class [ LoadingEditorLine ]
        , style [ ( "width", (toString (width * 100)) ++ "%" ) ]
        ]
        []


loadingLines : Html msg
loadingLines =
    div [ class [ LoadingLines ] ]
        [ loadingLine 0.5
        , loadingLine 0.8
        , loadingLine 0.4
        , loadingLine 0.3
        , loadingLine 0
        , loadingLine 0
        , loadingLine 0.2
        , loadingLine 0.8
        , loadingLine 0.7
        , loadingLine 0.1
        , loadingLine 0.6
        ]


loadingLineNumber : Int -> Html msg
loadingLineNumber i =
    span [ class [ LoadingEditorLineNumber ] ]
        [ text <| toString i
        ]


loadingGutter : Html msg
loadingGutter =
    div [ class [ LoadingEditorGutter ] ]
        (List.range 1 12 |> List.map loadingLineNumber)


loadingShimmer : Html msg
loadingShimmer =
    div [ class [ LoadingShimmer ] ]
        []


loading : Html msg
loading =
    div [ class [ LoadingEditor ] ]
        [ loadingGutter
        , loadingLines
        , loadingShimmer
        ]


elm : (String -> msg) -> String -> List CompileError -> Html msg
elm onUpdate content compileErrors =
    let
        compileErrorLevelToSeverity level =
            case level of
                "warning" ->
                    CodeMirror.Warning

                _ ->
                    CodeMirror.Error

        actualRegion compileError =
            compileError.subregion
                |> Maybe.withDefault compileError.region

        compileErrorToLinterMessage compileError =
            let
                region =
                    actualRegion compileError
            in
                CodeMirror.linterMessage
                    (compileErrorLevelToSeverity compileError.level)
                    (compileError.overview ++ "\n\n" ++ compileError.details)
                    (CodeMirror.position (region.start.line - 1) (compileError.region.start.column - 1))
                    (CodeMirror.position (region.end.line - 1) (compileError.region.end.column))

        linterMessages =
            List.map compileErrorToLinterMessage compileErrors
    in
        CodeMirror.editor
            [ value content
            , CodeMirror.linterMessages linterMessages
            , CodeMirror.onUpdate onUpdate
            , CodeMirror.theme "yeti"
            , style
                [ ( "height", "100%" )
                , ( "width", "100%" )
                ]
            ]


html : (String -> msg) -> String -> Html msg
html onUpdate content =
    CodeMirror.editor
        [ value content
        , CodeMirror.onUpdate onUpdate
        , CodeMirror.theme "yeti"
        , style
            [ ( "height", "100%" )
            , ( "width", "100%" )
            ]
        ]
