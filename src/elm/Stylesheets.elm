port module Stylesheets exposing (..)

import Css.File exposing (..)
import Css exposing (..)
import Css.Elements exposing (..)
import App.Styles as App
import Components.Sidebar.Styles as Sidebar
import Components.Output.Styles as Output
import Components.Editors.Styles as Editors
import Components.Header.Styles as Header
import Shared.Constants as Constants


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
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
