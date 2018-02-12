module Pages.Editor.Views.Settings exposing (Config, view)

import Colors
import Css exposing (..)
import Ellie.Types.Settings as Settings exposing (Settings)
import Ellie.Ui.Checkbox as Checkbox
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Switch as Switch
import Ellie.Ui.TextInput as TextInput
import Ellie.Ui.Theme as Theme
import Elm.Package as Package exposing (Package)
import Elm.Package.Name as Name
import Elm.Package.Searchable as Searchable exposing (Searchable)
import Elm.Package.Version as Version
import Extra.Maybe as Maybe
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css, src)


type alias Config msg =
    { settings : Settings
    , projectName : String
    , onSettingsChange : Settings -> msg
    , onProjectNameChange : String -> msg
    }


view : Config msg -> Html msg
view config =
    Html.div []
        [ Html.h3
            [ css
                [ margin zero
                , padding (px 16)
                , paddingBottom (px 8)
                , fontSize (px 14)
                , fontWeight bold
                , textTransform uppercase
                , color Theme.primaryForeground
                ]
            ]
            [ Html.text "Settings" ]
        , Html.div
            [ css [ padding (px 8) ]
            ]
            [ viewProjectName config
            , viewVimMode config
            , viewFontFamily config
            , viewFontSize config
            , viewTheme config
            ]
        ]


viewProjectName : Config msg -> Html msg
viewProjectName { projectName, onProjectNameChange } =
    Html.div [ settingContainerStyles ]
        [ Html.div [ settingHeaderStyles ] [ Html.text "Project Name" ]
        , Html.div [ settingDescriptionStyles ] [ Html.text "Give your project a name" ]
        , Html.div [ settingControlStyles ]
            [ Html.div
                [ css [ backgroundColor Theme.secondaryBackground ]
                ]
                [ TextInput.view
                    { placeholder = "Untitled"
                    , value = projectName
                    , clearable = False
                    , icon = Nothing
                    , onChange = onProjectNameChange
                    }
                ]
            ]
        ]


viewVimMode : Config msg -> Html msg
viewVimMode { settings, onSettingsChange } =
    Html.div [ settingContainerStyles ]
        [ Html.div [ settingHeaderStyles ] [ Html.text "Vim Mode" ]
        , Html.div [ settingDescriptionStyles ] [ Html.text "Use vim key bindings in the text editors" ]
        , Html.div [ settingControlStyles ]
            [ Checkbox.view
                { id = "settings-vim-mode"
                , checked = settings.vimMode
                , label = Html.span [ settingControlLabelStyles ] [ Html.text "Enabled" ]
                , onChange = \vimMode -> onSettingsChange { settings | vimMode = vimMode }
                }
            ]
        ]


viewFontFamily : Config msg -> Html msg
viewFontFamily { settings, onSettingsChange } =
    Html.div [ settingContainerStyles ]
        [ Html.div [ settingHeaderStyles ] [ Html.text "Editor Font Family" ]
        , Html.div [ settingDescriptionStyles ] [ Html.text "Choose any installed font to use in the editors" ]
        , Html.div [ settingControlStyles ]
            [ Html.div
                [ css
                    [ fontFamilies [ settings.fontFamily ]
                    , backgroundColor Theme.secondaryBackground
                    ]
                ]
                [ TextInput.view
                    { placeholder = "monospace"
                    , value = settings.fontFamily
                    , clearable = False
                    , icon = Nothing
                    , onChange = \fontFamily -> onSettingsChange { settings | fontFamily = fontFamily }
                    }
                ]
            ]
        ]


viewFontSize : Config msg -> Html msg
viewFontSize { settings, onSettingsChange } =
    Html.div [ settingContainerStyles ]
        [ Html.div [ settingHeaderStyles ] [ Html.text "Editor Font Size" ]
        , Html.div [ settingDescriptionStyles ] [ Html.text "The size of the text in the code editors" ]
        , Html.div [ settingControlStyles ]
            [ Html.div
                [ css [ backgroundColor Theme.secondaryBackground ]
                ]
                [ TextInput.view
                    { placeholder = "16px"
                    , value = settings.fontSize
                    , clearable = False
                    , icon = Nothing
                    , onChange = \fontSize -> onSettingsChange { settings | fontSize = fontSize }
                    }
                ]
            ]
        ]


viewTheme : Config msg -> Html msg
viewTheme { settings, onSettingsChange } =
    Html.div [ settingContainerStyles ]
        [ Html.div [ settingHeaderStyles ] [ Html.text "Color Theme" ]
        , Html.div [ settingDescriptionStyles ] [ Html.text "Choose a dark or light theme for Ellie" ]
        , Html.div [ settingControlStyles ]
            [ Switch.view
                { id = "settings-theme"
                , onLabel = "Light"
                , offLabel = "Dark"
                , on =
                    case settings.theme of
                        Settings.Dark ->
                            False

                        Settings.Light ->
                            True
                , onChange =
                    \on ->
                        onSettingsChange
                            { settings
                                | theme =
                                    if on then
                                        Settings.Light
                                    else
                                        Settings.Dark
                            }
                }
            ]
        ]



-- STYLES


settingContainerStyles : Attribute msg
settingContainerStyles =
    css
        [ backgroundColor Theme.primaryBackground
        , padding (px 16)
        , marginBottom (px 8)
        , overflow hidden
        ]


settingHeaderStyles : Attribute msg
settingHeaderStyles =
    css
        [ fontSize (px 16)
        , lineHeight (num 1)
        , fontWeight bold
        , color Theme.primaryForeground
        , paddingBottom (px 8)
        ]


settingDescriptionStyles : Attribute msg
settingDescriptionStyles =
    css
        [ fontSize (px 14)
        , lineHeight (num 1)
        , color Theme.secondaryForeground
        , paddingBottom (px 8)
        ]


settingControlLabelStyles : Attribute msg
settingControlLabelStyles =
    css
        [ fontSize (px 16)
        , fontWeight bold
        , color Theme.primaryForeground
        ]


settingControlStyles : Attribute msg
settingControlStyles =
    css
        []
