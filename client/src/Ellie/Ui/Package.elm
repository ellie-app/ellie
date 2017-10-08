module Ellie.Ui.Package exposing (Action, Config, install, uninstall, view)

import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Package.Styles as Styles
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)


type Action msg
    = Install msg
    | Uninstall msg


install : msg -> Action msg
install =
    Install


uninstall : msg -> Action msg
uninstall =
    Uninstall


type alias Config msg =
    { package : Package
    , action : Action msg
    }


view : Config msg -> Html msg
view config =
    let
        ( name, version ) =
            config.package
    in
    div [ Styles.container ]
        [ div [ Styles.details ]
            [ div [ Styles.project ] [ text name.project ]
            , div [ Styles.detailsLine ]
                [ div [ Styles.infoIcon ] [ Icon.view Icon.Tag ]
                , div [ Styles.infoText ] [ text <| Version.toString version ]
                ]
            , div [ Styles.detailsLine ]
                [ img
                    [ src <| "https://github.com/" ++ name.user ++ ".png?size=14"
                    , Styles.infoIcon
                    ]
                    []
                , div [ Styles.infoText ] [ text name.user ]
                ]
            ]
        , div [ Styles.buttons ]
            [ case config.action of
                Uninstall action ->
                    Button.view
                        { size = Button.Small
                        , icon = Just Icon.Trash
                        , disabled = False
                        , action = Button.click action
                        , label = "Remove"
                        , attributes = []
                        , style = Button.Link
                        }

                Install action ->
                    Button.view
                        { size = Button.Small
                        , icon = Just Icon.Install
                        , disabled = False
                        , action = Button.click action
                        , attributes = []
                        , label = "Install"
                        , style = Button.Accent
                        }
            , Button.view
                { size = Button.Small
                , style = Button.Link
                , icon = Just Icon.Document
                , label = "Docs"
                , disabled = False
                , attributes = []
                , action =
                    Button.link
                        { href = Package.docsLink config.package
                        , external = True
                        }
                }
            , Button.view
                { size = Button.Small
                , style = Button.Link
                , icon = Just Icon.Fork
                , label = "Code"
                , disabled = False
                , attributes = []
                , action =
                    Button.link
                        { href = Package.codeLink config.package
                        , external = True
                        }
                }
            ]
        ]
