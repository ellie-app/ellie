port module Stylesheets exposing (..)

import Css.File exposing (..)
import Css exposing (..)
import Css.Elements exposing (..)
import Views.Styles as Views
import Classes exposing (Classes(..))


styles : Stylesheet
styles =
    (stylesheet)
        [ html
            [ height (pct 100) ]
        , body
            [ height (pct 100)
            , margin zero
            , property "font-family" "sans-serif"
            , fontWeight (int 300)
            ]
        , everything
            [ boxSizing borderBox ]
        , (.) EditorsContainer
            [ displayFlex
            , flexDirection column
            , borderRight3 (px 1) solid (hex "dddddd")
            ]
        , (.) EditorContainer
            [ width (pct 100)
            , overflow hidden
            ]
        , (.) EditorsSeparator
            [ width (pct 100)
            , height (px 5)
            , top (px -2)
            , marginBottom (px -5)
            , property "z-index" "3"
            , cursor nsResize
            ]
        , (.) WorkAreaContainer
            [ displayFlex
            , height (pct 100)
            , width (pct 100)
            ]
        , (.) ResultsEditorsSeparator
            [ position relative
            , height (pct 100)
            , width (px 5)
            , right (px -2)
            , marginLeft (px 5)
            , cursor ewResize
            , property "z-index" "3"
            ]
        , (.) ResultsContainer
            [ displayFlex
            , height (pct 100)
            ]
        ]


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css", Css.File.compile [ styles, Views.styles ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
