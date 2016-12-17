port module Stylesheets exposing (..)

import Css.File exposing (..)
import App.Styles as App
import Components.Sidebar.Styles as Sidebar
import Components.Output.Styles as Output
import Components.Editors.Styles as Editors
import Components.Header.Styles as Header
import Components.PackageSearch.Styles as PackageSearch


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css"
          , Css.File.compile
                [ App.styles
                , Sidebar.styles
                , Output.styles
                , Editors.styles
                , Header.styles
                , PackageSearch.styles
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
