port module Stylesheets exposing (..)

import Css.File exposing (..)
import Apps.Editor.Styles as App
import Components.Sidebar.Styles as Sidebar
import Components.Output.Styles as Output
import Components.Editors.Styles as Editors
import Components.Header.Styles as Header
import Components.Search.Styles as Search
import Components.Notifications.Styles as Notifications
import Components.About.Styles as About
import Components.Splash.Styles as Splash
import Components.Editor.EmbedLink.Styles as EmbedLink


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
                , Splash.styles
                , About.styles
                , EmbedLink.styles
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
