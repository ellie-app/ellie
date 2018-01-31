module Pages.Editor.Sidebar.View exposing (..)

import Colors
import Css exposing (..)
import Css.Foreign
import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Data.Elm.Package as Package exposing (Package)
import Data.List.Iterator as Iterator exposing (Iterator(..))
import Ellie.Ui.Button as Button
import Ellie.Ui.Checkbox as Checkbox
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Package as PackageView
import Ellie.Ui.Sections as Sections
import Ellie.Ui.Setting as Setting
import Ellie.Ui.TextArea as TextArea
import Ellie.Ui.TextInput as TextInput
import Html.Styled exposing (Html, Attribute, a, div, p, span, text)
import Html.Styled.Attributes exposing (css, href)
import Pages.Editor.Sidebar.Model as Model exposing (Model)
import Pages.Editor.Sidebar.Update exposing (Msg(..))



type alias Config msg =
    { title : String
    , onTitleChange : String -> msg
    , description : String
    , vimMode : Bool
    , onDescriptionChange : String -> msg
    , onClearElmStuff : msg
    , onVimModeChange : Bool -> msg
    , installed : List Package
    , onPackageRemoved : Package -> msg
    , onPackageAdded : Package -> msg
    , latestTerms : TermsVersion
    , mapMsg : Msg -> msg
    , model : Model
    }


viewSettings : Config msg -> () -> Html msg
viewSettings config () =
    div [ settingsStyles ]
        [ div [ settingStyles ]
            [ Setting.view
                { label = "Project Title"
                , description = "Give your project a name."
                , control =
                    TextInput.view
                        { placeholder = "Title goes here."
                        , value = config.title
                        , clearable = False
                        , icon = Nothing
                        , onChange = config.onTitleChange
                        }
                }
            ]
        , div [ settingStyles ]
            [ Setting.view
                { label = "Project Description"
                , description = "Tell the world about your project."
                , control =
                    TextArea.view
                        { placeholder = "Description goes here."
                        , value = config.description
                        , onChange = config.onDescriptionChange
                        }
                }
            ]
        , div [ settingStyles ]
            [ Setting.view
                { label = "Vim Mode"
                , description = "Use vim keybindings in the editors"
                , control =
                    Checkbox.view
                        { onChange = config.onVimModeChange
                        , checked = config.vimMode
                        , label = span [ vimModeLabelStyles ] [ text "Enabled" ]
                        , id = "vimMode"
                        }
                }
            ]
        , div [ settingStyles ]
            [ Setting.view
                { label = "Clear elm-stuff"
                , description = "Clearing elm-stuff can help if you are having issues with compiling."
                , control =
                    Button.view
                        { size = Button.Medium
                        , style = Button.Primary
                        , icon = Nothing
                        , label = "Clear elm-stuff"
                        , disabled = False
                        , attributes = []
                        , action = Button.click config.onClearElmStuff
                        }
                }
            ]
        ]


settingsSection : Config msg -> Sections.Section msg
settingsSection config =
    { title = "Settings"
    , icon = Icon.Settings
    , onSelect = config.mapMsg (ChangePanel Model.Settings)
    , content = viewSettings config
    }


viewPackages : Config msg -> () -> Html msg
viewPackages config () =
    div [ packagesStyles ]
        [ div [ searchStyles ]
            [ div [ packagesSectionTitleStyles ]
                [ if String.length config.model.search > 3 then
                    text "Searching for…"
                  else
                    text "Search for packages"
                ]
            , TextInput.view
                { placeholder = "user/project"
                , value = config.model.search
                , clearable = True
                , icon = Nothing
                , onChange = SearchChanged >> config.mapMsg
                }
            ]
        , if String.length config.model.search /= 0 then
            div [] <|
                (config.model.results
                    |> List.filter (\( name, _ ) -> not <| List.any (Tuple.first >> (==) name) config.installed)
                    |> List.map
                        (\p ->
                            div [ packageStyles ]
                                [ PackageView.view
                                    { package = p
                                    , action = PackageView.install <| config.onPackageAdded p
                                    }
                                ]
                        )
                )
          else
            div []
                [ div [ packagesSectionTitleStyles ]
                    [ text "Installed Packages" ]
                , div [] <|
                    List.map
                        (\p ->
                            div [ packageStyles ]
                                [ PackageView.view
                                    { package = p
                                    , action = PackageView.uninstall <| config.onPackageRemoved p
                                    }
                                ]
                        )
                        config.installed
                ]
        ]


packagesSection : Config msg -> Sections.Section msg
packagesSection config =
    { title = "Packages"
    , icon = Icon.Package
    , onSelect = config.mapMsg (ChangePanel Model.Packages)
    , content = viewPackages config
    }


viewAbout : Config msg -> () -> Html msg
viewAbout config () =
    div [ aboutStyles ]
        [ div [ aboutHeadingStyles ]
            [ text "Ellie is the Elm platform in your browser." ]
        , p [ aboutParagraphStyles ]
            [ text "With Ellie you can use all of Elm’s features to build amazing animations, precise SSCCEs, cool demos, and anything else you could create with Elm in an ordinary development environment."
            ]
        , p [ aboutParagraphStyles ]
            [ text "Add packages in the sidebar, write a program, work through compiler errors, and share your work with the world."
            ]
        , p [ aboutParagraphStyles ]
            [ text "All content created with Ellie is released under the "
            , a [ href "https://opensource.org/licenses/MIT" ] [ text "MIT license" ]
            , text ". We reserve the right to remove or modify any content created with Ellie for any reason. Report abuse and ask questions at "
            , a [ href "mailto:ellie-app@lukewestby.com" ] [ text "ellie-app@lukewestby.com" ]
            , text "."
            ]
        , p [ aboutParagraphStyles ]
            [ text "Our latest terms of service can be found "
            , a [ href <| TermsVersion.link config.latestTerms ] [ text "here" ]
            , text "."
            ]
        , p [ aboutCopyrightStyles ]
            [ text "© 2017 Luke Westby" ]
        ]


aboutSection : Config msg -> Sections.Section msg
aboutSection config =
    { title = "About"
    , icon = Icon.Info
    , onSelect = config.mapMsg (ChangePanel Model.About)
    , content = viewAbout config
    }


view : Config msg -> Html msg
view config =
    div [ containerStyles ]
        [ div [ sectionsStyles ]
            [ Sections.view <| toIterator config ]
        ]


toIterator : Config msg -> Iterator (Sections.Section msg)
toIterator config =
    case config.model.panel of
        Just Model.Packages ->
            Iterator
                []
                (Just <| packagesSection config)
                [ settingsSection config
                , aboutSection config
                ]

        Just Model.Settings ->
            Iterator
                [ packagesSection config ]
                (Just <| settingsSection config)
                [ aboutSection config ]

        Just Model.About ->
            Iterator
                [ settingsSection config, packagesSection config ]
                (Just <| aboutSection config)
                []

        Nothing ->
            Iterator.fromList <|
                [ packagesSection config, settingsSection config, aboutSection config ]


-- STYLES



vimModeLabelStyles : Attribute msg
vimModeLabelStyles =
    css
        [ color Colors.lightMediumGray ]


sectionsStyles : Attribute msg
sectionsStyles =
    css
        [ position relative
        , flexShrink (int 1)
        , height (pct 100)
        , overflowY hidden
        , width (pct 100)
        , displayFlex
        , flexDirection column
        ]


containerStyles : Attribute msg
containerStyles =
    css
        [ position relative
        , width (pct 100)
        , backgroundColor Colors.darkGray
        , displayFlex
        , flexDirection column
        , alignItems center
        , height (pct 100)
        ]


settingsStyles : Attribute msg
settingsStyles =
    css
        [ position relative
        , padding2 zero (px 12)
        ]


settingStyles : Attribute msg
settingStyles =
    css
        [ margin2 (px 16) zero ]


packagesSectionTitleStyles : Attribute msg
packagesSectionTitleStyles =
    css
        [ fontSize (px 14)
        , color Colors.lightGray
        , paddingBottom (px 8)
        ]


packagesStyles : Attribute msg
packagesStyles =
    css
        [ padding (px 12)
        ]


searchStyles : Attribute msg
searchStyles =
    css
        [ marginBottom (px 12) ]


packageStyles : Attribute msg
packageStyles =
    css
        [ marginBottom (px 12)
        , lastChild
            [ marginBottom zero ]
        ]


aboutStyles : Attribute msg
aboutStyles =
    css
        [ padding2 (px 16) (px 12)
        ]


aboutHeadingStyles : Attribute msg
aboutHeadingStyles =
    css
        [ fontSize (px 16)
        , fontWeight bold
        , color Colors.lightGray
        ]


aboutParagraphStyles : Attribute msg
aboutParagraphStyles =
    css
        [ fontSize (px 14)
        , color Colors.lightGray
        , Css.Foreign.descendants
            [ Css.Foreign.a [ color Colors.pink, textDecoration underline ]
            ]
        ]


aboutCopyrightStyles : Attribute msg
aboutCopyrightStyles =
    css
        [ fontSize (px 12)
        , color Colors.lightMediumGray
        ]
