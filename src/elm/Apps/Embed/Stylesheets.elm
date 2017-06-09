port module Stylesheets exposing (..)

import Css.File exposing (..)
import Apps.Embed.Styles as App
import Views.Output.Styles as Output


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css"
          , Css.File.compile
                [ App.styles
                , Output.styles
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
