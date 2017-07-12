port module Pages.Editor.Stylesheets exposing (..)

import Css.File exposing (..)
import Pages.Editor.Styles as Editor
import Views.About.Styles as About
import Views.Editor.EmbedLink.Styles as EmbedLink
import Views.Editors.Styles as Editors
import Views.Header.Styles as Header
import Views.Notifications.Styles as Notifications
import Views.Output.Styles as Output
import Views.ProgressBar.Styles as ProgressBar
import Views.Search.Styles as Search
import Views.Sidebar.Styles as Sidebar


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css"
          , Css.File.compile
                [ Editor.styles
                , Sidebar.styles
                , Output.styles
                , Editors.styles
                , Header.styles
                , Search.styles
                , Notifications.styles
                , About.styles
                , EmbedLink.styles
                , ProgressBar.styles
                ]
          )
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
