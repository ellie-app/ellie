port module Pages.Embed.Stylesheets exposing (..)

import Css.File exposing (..)
import Pages.Embed.Styles as Embed
import Views.Output.Styles as Output


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css"
          , Css.File.compile
                [ Embed.styles
                , Output.styles
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
