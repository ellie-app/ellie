module Pages.Editor.Views.Editors exposing (..)

import Css exposing (..)
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Menu as Menu
import Ellie.Ui.SplitPane as SplitPane
import Ellie.Ui.Theme as Theme
import Elm.Compiler.Error as Error exposing (Error)
import Extra.Markdown as Markdown
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Pages.Editor.Types.Example as Example exposing (Example)


type alias Config msg =
    { elmCode : String
    , onElmChange : String -> msg
    , htmlCode : String
    , onHtmlChange : String -> msg
    , onResize : Float -> msg
    , onExampleSelect : Example -> msg
    , ratio : Float
    , vimMode : Bool
    , onFormat : msg
    , onCollapse : msg
    , elmErrors : List Error
    }


errorToLinterMessage : Error -> CodeEditor.LinterMessage
errorToLinterMessage error =
    { from = { line = error.region.start.line - 1, column = error.region.start.column - 1 }
    , to = { line = error.region.end.line - 1, column = error.region.end.column - 1 }
    , message = Markdown.toString error.message
    , severity =
        case error.level of
            "warning" ->
                CodeEditor.Warning

            _ ->
                CodeEditor.Error
    }


view : Config msg -> Html msg
view config =
    Html.node "ellie-editors"
        [ css
            [ display block
            , height (pct 100)
            ]
        ]
        [ SplitPane.view
            { direction = SplitPane.Vertical
            , ratio = config.ratio
            , originalRatio = 0.75
            , onResize = config.onResize
            , minSize = 24
            , first =
                Html.div [ containerStyles ]
                    [ viewEditorHeader config "Elm" "Format Elm Code (⇧⌥F)" config.onFormat <| Icon.view Icon.Format
                    , Html.div [ editorStyles ]
                        [ CodeEditor.view
                            [ CodeEditor.value config.elmCode
                            , CodeEditor.onChange config.onElmChange
                            , CodeEditor.mode "elm"
                            , CodeEditor.tabSize 4
                            , CodeEditor.vim config.vimMode
                            , CodeEditor.linterMessages <| List.map errorToLinterMessage config.elmErrors
                            , CodeEditor.id "elm"
                            ]
                        ]
                    ]
            , second =
                Html.div [ containerStyles ]
                    [ viewEditorHeader config "HTML" "Collapse HTML Editor" config.onCollapse <|
                        if config.ratio == 1 then
                            Html.div
                                [ css [ height (pct 100), transform (rotate (deg 180)) ] ]
                                [ Icon.view Icon.Chevron ]
                        else
                            Icon.view Icon.Chevron
                    , Html.div [ editorStyles ]
                        [ CodeEditor.view
                            [ CodeEditor.value config.htmlCode
                            , CodeEditor.onChange config.onHtmlChange
                            , CodeEditor.mode "htmlmixed"
                            , CodeEditor.tabSize 2
                            , CodeEditor.vim config.vimMode
                            , CodeEditor.id "html"
                            ]
                        ]
                    ]
            }
        ]


editorStyles : Attribute msg
editorStyles =
    css
        [ flexShrink (int 1)
        , height (pct 100)
        , width (pct 100)
        , displayFlex
        ]


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , displayFlex
        , flexDirection column
        , height (pct 100)
        , overflow hidden
        ]


viewEditorHeader : Config msg -> String -> String -> msg -> Html msg -> Html msg
viewEditorHeader config name tooltip msg icon =
    Html.div
        [ css
            [ backgroundColor Theme.editorHeaderBackground
            , displayFlex
            , justifyContent spaceBetween
            , alignItems center
            , padding2 zero (px 8)
            , height (px 24)
            , flexShrink (int 0)
            ]
        ]
        [ Html.div
            [ css
                [ fontSize (px 14)
                , lineHeight (num 1)
                , textTransform uppercase
                , fontWeight bold
                , color Theme.primaryForeground
                ]
            ]
            [ Html.text name ]
        , Html.div
            [ css
                [ displayFlex
                , alignItems center
                , justifyContent flexEnd
                ]
            ]
            [ Html.div
                [ css
                    [ width (px 24)
                    , height (px 24)
                    , position relative
                    ]
                ]
                [ Menu.view
                    { icon = Icon.More
                    , items = List.map (\e -> { label = e.label, onSelect = config.onExampleSelect e }) Example.all
                    }
                ]
            , Html.button
                [ css
                    [ property "background" "none"
                    , border zero
                    , outline zero
                    , display block
                    , width (px 24)
                    , height (px 24)
                    , padding (px 6)
                    , color Theme.secondaryForeground
                    , cursor pointer
                    , hover
                        [ color Theme.primaryForeground
                        ]
                    , active
                        [ transform <| scale 1.2
                        ]
                    ]
                , Events.onClick msg
                , Attributes.title tooltip
                ]
                [ icon ]
            ]
        ]
