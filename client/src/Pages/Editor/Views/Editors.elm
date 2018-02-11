module Pages.Editor.Views.Editors exposing (..)

import Css exposing (..)
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.SplitPane as SplitPane
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


type alias Config msg =
    { elmCode : String
    , onElmChange : String -> msg
    , htmlCode : String
    , onHtmlChange : String -> msg
    , ratio : Float
    , onResize : Float -> msg
    }


view : Config msg -> Html msg
view { onElmChange, elmCode, onHtmlChange, htmlCode, ratio, onResize } =
    Html.node "ellie-editors"
        [ css [ display block ] ]
        [ SplitPane.view
            { direction = SplitPane.Vertical
            , ratio = ratio
            , onResize = onResize
            , first =
                CodeEditor.view
                    [ CodeEditor.value elmCode
                    , CodeEditor.onChange onElmChange
                    , CodeEditor.mode "elm"
                    , CodeEditor.tabSize 4
                    ]
            , second =
                CodeEditor.view
                    [ CodeEditor.value htmlCode
                    , CodeEditor.onChange onHtmlChange
                    , CodeEditor.mode "htmlmixed"
                    , CodeEditor.tabSize 2
                    ]
            }
        ]
