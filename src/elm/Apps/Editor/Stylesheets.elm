port module Stylesheets exposing (..)

import Css.File exposing (..)
import Apps.Editor.Styles as App
import Views.Sidebar.Styles as Sidebar
import Views.Output.Styles as Output
import Views.Editors.Styles as Editors
import Views.Header.Styles as Header
import Views.Search.Styles as Search
import Views.Notifications.Styles as Notifications
import Views.About.Styles as About
import Views.Editor.EmbedLink.Styles as EmbedLink


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
                , Search.styles
                , Notifications.styles
                , About.styles
                , EmbedLink.styles
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
