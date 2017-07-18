module Views.Editors.View
    exposing
        ( elm
        , html
        , loading
        )

import Data.Elm.Compiler.Error as CompilerError
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style, value)
import Shared.Utils as Utils
import Views.Editors.Classes exposing (Classes(..), class)
import Views.Editors.CodeMirror as CodeMirror


singleton : a -> List a
singleton a =
    [ a ]


loadingLine : Float -> Html msg
loadingLine width =
    div
        [ class [ LoadingLine ]
        , style [ ( "width", toString (width * 100) ++ "%" ) ]
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
    span [ class [ LoadingLineNumber ] ]
        [ text <| toString i
        ]


loadingGutter : Html msg
loadingGutter =
    div [ class [ LoadingGutter ] ]
        (List.range 1 12 |> List.map loadingLineNumber)


loadingShimmer : Html msg
loadingShimmer =
    div [ class [ LoadingShimmer ] ]
        []


loading : Html msg
loading =
    div [ class [ Loading ] ]
        [ loadingGutter
        , loadingLines
        , loadingShimmer
        ]


elm : Bool -> Maybe (String -> msg) -> String -> List CompilerError.Error -> Html msg
elm vimMode onUpdate content compileErrors =
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
                (Utils.replaceAll <| compileError.overview ++ "\n\n" ++ compileError.details)
                (CodeMirror.position (region.start.line - 1) (compileError.region.start.column - 1))
                (CodeMirror.position (region.end.line - 1) compileError.region.end.column)

        linterMessages =
            List.map compileErrorToLinterMessage compileErrors

        baseAttrs =
            [ CodeMirror.value content
            , CodeMirror.linterMessages linterMessages
            , CodeMirror.theme "material"
            , CodeMirror.mode "elm"
            , CodeMirror.vimMode vimMode
            , CodeMirror.indentWidth 4
            , style
                [ ( "height", "100%" )
                , ( "width", "100%" )
                ]
            ]

        updateAttrs =
            onUpdate
                |> Maybe.map (\u -> [ CodeMirror.onUpdate u, CodeMirror.readOnly False ])
                |> Maybe.withDefault [ CodeMirror.readOnly True ]
    in
    CodeMirror.editor (baseAttrs ++ updateAttrs)


html : Bool -> Maybe (String -> msg) -> String -> Html msg
html vimMode onUpdate content =
    let
        baseAttrs =
            [ CodeMirror.value content
            , CodeMirror.theme "material"
            , CodeMirror.mode "htmlmixed"
            , CodeMirror.vimMode vimMode
            , CodeMirror.indentWidth 2
            , style
                [ ( "height", "100%" )
                , ( "width", "100%" )
                ]
            ]

        updateAttrs =
            onUpdate
                |> Maybe.map (\u -> [ CodeMirror.onUpdate u, CodeMirror.readOnly False ])
                |> Maybe.withDefault [ CodeMirror.readOnly True ]
    in
    CodeMirror.editor (baseAttrs ++ updateAttrs)
