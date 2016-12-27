module Components.Sidebar.View
    exposing
        ( Context
        , view
        )

import Json.Decode as Decode
import List.Nonempty as Nonempty
import RemoteData exposing (RemoteData(..))
import Html exposing (Html, textarea, h3, button, aside, div, text, span, input, hr, option, select)
import Html.Attributes exposing (type_, value, selected, placeholder)
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
    , title : String
    , onTitleChanged : String -> msg
    , description : String
    , onDescriptionChanged : String -> msg
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
            , class [ IconButton ]
            ]
            [ Icons.close ]
        ]


packageFlowItem : (PackageSearchResult -> msg) -> PackageSearchResult -> Html msg
packageFlowItem onSelected r =
    div
        [ class [ AddDepPackageItem ]
        , onClick (onSelected r)
        ]
        [ text <| r.username ++ "/" ++ r.name ]


notSearching : Context msg -> Html msg
notSearching context =
    button
        [ onClick context.onStarted
        , class [ AddDepButton ]
        ]
        [ span [] [ text "Add Dependency" ]
        , span [ class [ AddDepButtonIcon ] ] [ Icons.plusEmpty ]
        ]


packageSearch : Context msg -> String -> List PackageSearchResult -> Html msg
packageSearch context searchText packages =
    div [ class [ AddDepPackageSearch ] ]
        [ div [ class [ AddDepContainer ] ]
            [ div [ class [ AddDepInputContainer ] ]
                [ input
                    [ type_ "text"
                    , onInput context.onSearchTermChanged
                    , class [ TextInput ]
                    , placeholder "Search packages..."
                    ]
                    []
                ]
            , button
                [ onClick context.onCancelled
                , class [ IconButton ]
                ]
                [ Icons.close ]
            ]
        , case packages of
            [] ->
                text ""

            _ ->
                div [ class [ AddDepResultList ] ]
                    (packages
                        |> Utils.hashFilter sharedHash sharedHash context.dependencies
                        |> List.map (packageFlowItem context.onPackageSelected)
                    )
        ]


versionSearch : Context msg -> PackageSearchResult -> Version -> Html msg
versionSearch context package version =
    div [ class [ AddDepVersion ] ]
        [ div [ class [ AddDepVersionDetails ] ]
            [ div [ class [ AddDepVersionPackageName ] ]
                [ text <| package.username ++ "/" ++ package.name
                ]
            , div []
                [ select
                    [ on "change" <| Decode.map context.onVersionSelected (Decode.at [ "target", "selectedIndex" ] Decode.int) ]
                    (Nonempty.toList <| Nonempty.map (\v -> option [ selected (version == v) ] [ text <| Version.toString v ]) package.versions)
                , span [] [ text " <= v < " ]
                , span [] [ text <| Version.toString (Version.nextMajor version) ]
                ]
            ]
        , button
            [ onClick context.onInstallRequested
            , class [ IconButton ]
            ]
            [ Icons.checkmark ]
        , button
            [ onClick context.onCancelled
            , class [ IconButton ]
            ]
            [ Icons.close ]
        ]


installation : Context msg -> Dependency -> RemoteData x a -> Html msg
installation context dependency data =
    case data of
        Loading ->
            div [] [ text <| "Installing..." ]

        _ ->
            text ""


newPackageFlow : Context msg -> Html msg
newPackageFlow context =
    case context.newPackageFlow of
        NotSearching ->
            notSearching context

        PackageSearch searchText packages ->
            packageSearch context searchText packages

        VersionSearch package version ->
            versionSearch context package version

        Installation dependency data ->
            installation context dependency data


view : Context msg -> Html msg
view context =
    aside [ class [ Sidebar ] ]
        [ div [ class [ Section ] ]
            [ h3 [ class [ SectionHeader ] ] [ text "Title" ]
            , div [ class [ SectionContent ] ]
                [ input
                    [ type_ "text"
                    , class [ TextInput ]
                    , value context.title
                    , onInput context.onTitleChanged
                    ]
                    []
                ]
            ]
        , div [ class [ Section ] ]
            [ h3 [ class [ SectionHeader ] ] [ text "Description" ]
            , div [ class [ SectionContent ] ]
                [ textarea
                    [ class [ Textarea ]
                    , onInput context.onDescriptionChanged
                    , value context.description
                    ]
                    []
                ]
            ]
        , div [ class [ Section ] ]
            [ h3 [ class [ SectionHeader ] ] [ text "Packages" ]
            , div [ class [ PackagesList ] ]
                (List.map (dependency context.onRemoved) context.dependencies)
            , div [ class [ SectionContent ] ] [ newPackageFlow context ]
            ]
        ]
