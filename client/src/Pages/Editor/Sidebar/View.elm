module Pages.Editor.Sidebar.View exposing (..)

import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Data.Elm.Package as Package exposing (Package)
import Ellie.Ui.Ad as Ad
import Ellie.Ui.Button as Button
import Ellie.Ui.Checkbox as Checkbox
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Package as PackageView
import Ellie.Ui.Sections as Sections
import Ellie.Ui.Setting as Setting
import Ellie.Ui.TextArea as TextArea
import Ellie.Ui.TextInput as TextInput
import Html exposing (Html, a, div, p, span, text)
import Html.Attributes exposing (href)
import List.Zipper as Zipper exposing (Zipper)
import Pages.Editor.Sidebar.Model as Model exposing (Model)
import Pages.Editor.Sidebar.Styles as Styles
import Pages.Editor.Sidebar.Update exposing (Msg(..))
import Shared.Constants as Constants


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
    div [ Styles.settings ]
        [ div [ Styles.setting ]
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
        , div [ Styles.setting ]
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
        , div [ Styles.setting ]
            [ Setting.view
                { label = "Vim Mode"
                , description = "Use vim keybindings in the editors"
                , control =
                    Checkbox.view
                        { onChange = config.onVimModeChange
                        , checked = config.vimMode
                        , label = span [ Styles.vimModeLabel ] [ text "Enabled" ]
                        , id = "vimMode"
                        }
                }
            ]
        , div [ Styles.setting ]
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
    div [ Styles.packages ]
        [ div [ Styles.search ]
            [ div [ Styles.packagesSectionTitle ]
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
                            div [ Styles.package ]
                                [ PackageView.view
                                    { package = p
                                    , action = PackageView.install <| config.onPackageAdded p
                                    }
                                ]
                        )
                )
          else
            div []
                [ div [ Styles.packagesSectionTitle ]
                    [ text "Installed Packages" ]
                , div [] <|
                    List.map
                        (\p ->
                            div [ Styles.package ]
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
    div [ Styles.about ]
        [ div [ Styles.aboutHeading ]
            [ text "Ellie is the Elm platform in your browser." ]
        , p [ Styles.aboutParagraph ]
            [ text "With Ellie you can use all of Elm’s features to build amazing animations, precise SSCCEs, cool demos, and anything else you could create with Elm in an ordinary development environment."
            ]
        , p [ Styles.aboutParagraph ]
            [ text "Add packages in the sidebar, write a program, work through compiler errors, and share your work with the world."
            ]
        , p [ Styles.aboutParagraph ]
            [ text "All content created with Ellie is released under the "
            , a [ href "https://opensource.org/licenses/MIT" ] [ text "MIT license" ]
            , text ". We reserve the right to remove or modify any content created with Ellie for any reason. Report abuse and ask questions at "
            , a [ href "mailto:ellie-app@lukewestby.com" ] [ text "ellie-app@lukewestby.com" ]
            , text "."
            ]
        , p [ Styles.aboutParagraph ]
            [ text "Our latest terms of service can be found "
            , a [ href <| TermsVersion.link config.latestTerms ] [ text "here" ]
            , text "."
            ]
        , p [ Styles.aboutCopyright ]
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
    div [ Styles.container ]
        [ div [ Styles.sections ]
            [ Sections.view <| toZipper config ]
        , div [ Styles.ad ]
            [ Ad.view
                { zoneId = Constants.carbonZoneId
                , serve = Constants.carbonServe
                , placement = Constants.carbonPlacement
                }
            ]
        ]


toZipper : Config msg -> Zipper (Sections.Section msg)
toZipper config =
    case config.model.panel of
        Model.Packages ->
            Zipper.Zipper
                []
                (packagesSection config)
                [ settingsSection config
                , aboutSection config
                ]

        Model.Settings ->
            Zipper.Zipper
                [ packagesSection config ]
                (settingsSection config)
                [ aboutSection config ]

        Model.About ->
            Zipper.Zipper
                [ settingsSection config, packagesSection config ]
                (aboutSection config)
                []
