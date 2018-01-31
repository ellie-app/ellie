module Ellie.Ui.Package exposing (Action, Config, install, uninstall, view)

import Colors
import Css exposing (..)
import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Html.Styled exposing (Html, Attribute, div, img, text)
import Html.Styled.Attributes exposing (css, src)


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
    div [ containerStyles ]
        [ div [ detailsStyles ]
            [ div [ projectStyles ] [ text name.project ]
            , div [ detailsLineStyles ]
                [ div [ infoIconStyles ] [ Icon.view Icon.Tag ]
                , div [ infoTextStyles ] [ text <| Version.toString version ]
                ]
            , div [ detailsLineStyles ]
                [ img
                    [ src <| "https://github.com/" ++ name.user ++ ".png?size=14"
                    , infoIconStyles
                    ]
                    []
                , div [ infoTextStyles ] [ text name.user ]
                ]
            ]
        , div [ buttonsStyles ]
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


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , width (pct 100)
        , displayFlex
        , backgroundColor Colors.darkMediumGray
        , Colors.boxShadow |> .bottom
        , borderRadius (px 2)
        , padding (px 8)
        ]


detailsStyles : Attribute msg
detailsStyles =
    css
        [ width (pct 100)
        , displayFlex
        , flexDirection column
        , justifyContent spaceBetween
        , height (pct 100)
        , paddingRight (px 4)
        , overflowX hidden
        ]


detailsLineStyles : Attribute msg
detailsLineStyles =
    css
        [ marginTop (px 6)
        , displayFlex
        , alignItems center
        ]


ellipsisTextStyles : Css.Style
ellipsisTextStyles =
    batch
        [ whiteSpace noWrap
        , overflowX hidden
        , textOverflow ellipsis
        ]


projectStyles : Attribute msg
projectStyles =
    css
        [ fontSize (px 16)
        , fontWeight bold
        , color Colors.lightGray
        , ellipsisTextStyles
        ]


infoIconStyles : Attribute msg
infoIconStyles =
    css
        [ width (px 14)
        , height (px 14)
        , marginRight (px 4)
        , color Colors.lightMediumGray
        ]


infoTextStyles : Attribute msg
infoTextStyles =
    css
        [ color Colors.lightMediumGray
        , fontSize (px 14)
        , ellipsisTextStyles
        ]


buttonsStyles : Attribute msg
buttonsStyles =
    css
        [ displayFlex
        , flexDirection column
        , justifyContent spaceBetween
        , flexShrink (int 0)
        ]
