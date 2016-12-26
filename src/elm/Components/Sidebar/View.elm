module Components.Sidebar.View
    exposing
        ( Context
        , view
        )

import Json.Decode as Decode
import List.Nonempty as Nonempty
import RemoteData exposing (RemoteData(..))
import Html exposing (Html, textarea, h3, button, aside, div, text, span, input, hr, option, select)
import Html.Attributes exposing (type_, value, selected)
import Html.Events exposing (onClick, onInput, on)
import Types.PackageSearchResult as PackageSearchResult exposing (PackageSearchResult)
import Types.Dependency as Dependency exposing (Dependency)
import Types.Version as Version exposing (Version)
import Types.NewPackageFlow as NewPackageFlow exposing (NewPackageFlow(..))
import Shared.Icons as Icons
import Shared.Utils as Utils
import Components.Sidebar.Classes exposing (..)


type alias Context msg =
    { dependencies : List Dependency
    , newPackageFlow : NewPackageFlow
    , onStarted : msg
    , onSearchTermChanged : String -> msg
    , onPackageSelected : PackageSearchResult -> msg
    , onVersionSelected : Int -> msg
    , onInstallRequested : msg
    , onCancelled : msg
    , onRemoved : Dependency -> msg
    }


sharedHash : { a | username : String, name : String } -> String
sharedHash a =
    a.username ++ "/" ++ a.name


dependency : (Dependency -> msg) -> Dependency -> Html msg
dependency onRemoved dep =
    div [ class [ DepItem ] ]
        [ div [ class [ DepItemDetails ] ]
            [ div [ class [ DepItemPackageName ] ]
                [ text <| dep.username ++ "/" ++ dep.name ]
            , div []
                [ text <| Version.toString dep.range.min
                , text <| " <= v < "
                , text <| Version.toString dep.range.max
                ]
            ]
        , button
            [ onClick (onRemoved dep)
            , class [ RemoveButton ]
            ]
            [ Icons.close ]
        ]


newPackageFlow : Context msg -> Html msg
newPackageFlow context =
    case context.newPackageFlow of
        NotSearching ->
            button
                [ onClick context.onStarted
                , class [ AddDepButton ]
                ]
                [ text "add dep" ]

        PackageSearch searchText packages ->
            div []
                [ div []
                    [ input
                        [ type_ "text"
                        , onInput context.onSearchTermChanged
                        ]
                        []
                    ]
                , div []
                    (packages
                        |> Utils.hashFilter sharedHash sharedHash context.dependencies
                        |> List.map (\r -> div [ onClick (context.onPackageSelected r) ] [ text <| r.username ++ "/" ++ r.name ])
                    )
                ]

        VersionSearch package version ->
            div []
                [ select [ on "change" <| Decode.map context.onVersionSelected (Decode.at [ "target", "selectedIndex" ] Decode.int) ] <|
                    Nonempty.toList <|
                        Nonempty.map (\v -> option [ selected (version == v) ] [ text <| Version.toString v ]) package.versions
                , button [ onClick context.onInstallRequested ] [ text "Install" ]
                ]

        Installation dependency data ->
            case data of
                NotAsked ->
                    text ""

                Loading ->
                    div []
                        [ text <| "Installing " ++ Dependency.toString dependency
                        ]

                Failure _ ->
                    div []
                        [ div [] [ text <| "Failed to install " ++ Dependency.toString dependency ]
                        , div [] [ button [ onClick context.onCancelled ] [ text "Cancel" ] ]
                        ]

                Success _ ->
                    div []
                        [ div [] [ text <| "Installed " ++ Dependency.toString dependency ]
                        , div [] [ button [ onClick context.onCancelled ] [ text "OK" ] ]
                        ]


view : Context msg -> Html msg
view context =
    aside [ class [ Sidebar ] ]
        [ div [ class [ Section ] ]
            [ h3 [ class [ SectionHeader ] ] [ text "Title" ]
            , div [ class [ SectionContent ] ]
                [ input
                    [ type_ "text"
                    , class [ TextInput ]
                    , value "Untitled"
                    ]
                    []
                ]
            ]
        , div [ class [ Section ] ]
            [ h3 [ class [ SectionHeader ] ] [ text "Description" ]
            , div [ class [ SectionContent ] ]
                [ textarea [ class [ Textarea ], value "Tell everyone about your project!" ] []
                ]
            ]
        , div [ class [ Section ] ]
            [ h3 [ class [ SectionHeader ] ] [ text "Packages" ]
            , div [ class [ PackagesList ] ]
                (List.map (dependency context.onRemoved) context.dependencies)
            , div [ class [ SectionContent ] ] [ newPackageFlow context ]
            ]
        ]
