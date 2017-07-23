port module Pages.Editor.Stylesheets exposing (..)

import Css.File exposing (..)
import Pages.Editor.Styles as Editor
import Views.Editor.About.Styles as About
import Views.Editor.EmbedLink.Styles as EmbedLink
import Views.Editor.Header.Styles as Header
import Views.Editor.Notifications.Styles as Notifications
import Views.Editor.Search.Styles as Search
import Views.Editor.Sidebar.Styles as Sidebar
import Views.Modal.Styles as Modal
import Views.Output.Styles as Output
import Views.ProgressBar.Styles as ProgressBar


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css"
          , Css.File.compile
                [ Editor.styles
                , Sidebar.styles
                , Output.styles
                , Header.styles
                , Search.styles
                , Notifications.styles
                , About.styles
                , EmbedLink.styles
                , ProgressBar.styles
                , Modal.styles
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
