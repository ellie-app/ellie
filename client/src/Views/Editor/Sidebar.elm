module Views.Editor.Sidebar
    exposing
        ( Config
        , view
        )

import Data.Elm.Package as Package exposing (Package)
import Data.Elm.Package.Version as Version exposing (Version)
import Html exposing (Attribute, Html, a, aside, button, div, h3, hr, input, label, node, option, select, span, text, textarea)
import Html.Attributes exposing (attribute, disabled, href, id, target, type_, value)
import Html.Events exposing (onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Shared.Icons as Icons
import Views.Editor.Ad as Ad
import Views.Editor.Sidebar.Styles as Styles


type alias Config msg =
    { title : String
    , description : String
    , onTitleChange : String -> msg
    , onDescriptionChange : String -> msg
    , packages : List Package
    , onAddPackageClick : msg
    , onRemove : Package -> msg
    }


viewProjectInfo : Config msg -> Html msg
viewProjectInfo config =
    div [ Styles.projectInfo ]
        [ div [ Styles.projectInfoTitle ]
            [ text "Project Info" ]
        , div []
            [ div [ Styles.projectInfoInputContainer ]
                [ label
                    [ Styles.projectInfoLabel ]
                    [ text "Title" ]
                , input
                    [ Styles.projectInfoInput
                    , type_ "text"
                    , value config.title
                    , onInput config.onTitleChange
                    ]
                    []
                ]
            , div [ Styles.projectInfoInputContainer ]
                [ label
                    [ Styles.projectInfoLabel ]
                    [ text "Description" ]
                , textarea
                    [ Styles.projectInfoTextarea
                    , value config.description
                    , onInput config.onDescriptionChange
                    ]
                    []
                ]
            ]
        ]


viewAddButton : Config msg -> Html msg
viewAddButton config =
    button
        [ Styles.addPackage
        , onClick config.onAddPackageClick
        ]
        [ text "Add A Package" ]


viewRemoveButton : msg -> Html msg
viewRemoveButton onRemove =
    button
        [ Styles.packagesItemButton
        , onWithOptions
            "click"
            { preventDefault = True, stopPropagation = True }
            (Decode.succeed onRemove)
        ]
        [ div [ Styles.packagesItemButtonInner ]
            [ span [ Styles.packagesItemButtonIcon ]
                [ Icons.close ]
            , span [ Styles.packagesItemButtonText ]
                [ text "Remove" ]
            ]
        ]


viewPackageItemVersion : Package -> Html msg
viewPackageItemVersion ( name, version ) =
    div [ Styles.packagesItemVersion ]
        [ text <| "@" ++ Version.toString version
        ]


viewPackageItemActions : Package -> Config msg -> Html msg
viewPackageItemActions package config =
    div [ Styles.packagesItemActions ]
        [ viewPackageItemVersion package
        , viewRemoveButton (config.onRemove package)
        ]


viewPackageItem : Config msg -> Package -> Html msg
viewPackageItem config (( name, version ) as package) =
    a
        [ Styles.packagesItem
        , href <| Package.docsLink package
        , target "_blank"
        ]
        [ div [ Styles.packagesItemName ]
            [ text <| name.user ++ " / " ++ name.project ]
        , viewPackageItemActions package config
        ]


viewPackages : Config msg -> Html msg
viewPackages config =
    div [ Styles.packages ]
        [ div [ Styles.packagesTitle ]
            [ text "Packages" ]
        , div [ Styles.packagesList ]
            (List.map (viewPackageItem config) config.packages)
        , viewAddButton config
        ]


viewTopStuff : Config msg -> Html msg
viewTopStuff config =
    div [ Styles.topStuff ]
        [ viewProjectInfo config
        , viewPackages config
        ]


viewBottomStuff : Html msg
viewBottomStuff =
    div [ Styles.bottomStuff ]
        [ Ad.view
        ]


view : Config msg -> Html msg
view config =
    aside [ Styles.sidebar ]
        [ viewTopStuff config
        , viewBottomStuff
        ]
