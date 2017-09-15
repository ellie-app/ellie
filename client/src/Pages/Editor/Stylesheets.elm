port module Pages.Editor.Stylesheets exposing (..)

import Css.File exposing (..)
import Extra.Css.Animation as Animation
import Pages.Editor.Styles as Editor
import Views.Button.Styles as Button
import Views.Editor.About.Styles as About
import Views.Editor.EmbedLink.Styles as EmbedLink
import Views.Editor.Header.Styles as Header
import Views.Editor.Notifications.Styles as Notifications
import Views.Editor.Search.Styles as Search
import Views.Editor.Sidebar.Styles as Sidebar
import Views.Editor.Terms.Styles as Terms
import Views.Modal.Styles as Modal
import Views.Output.Styles as Output
import Views.ProgressBar.Styles as ProgressBar


port files : CssFileStructure -> Cmd msg


styles : { warnings : List String, css : String }
styles =
    Css.File.compile
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
        , Terms.styles
        , Button.styles
        ]


keyFrames : String
keyFrames =
    Animation.compile
        [ Button.keyFrames
        ]


main : CssCompilerProgram
main =
    Css.File.compiler files <|
        Css.File.toFileStructure
            [ ( "index.css"
              , { styles | css = styles.css ++ "\n\n" ++ keyFrames }
              )
            ]
