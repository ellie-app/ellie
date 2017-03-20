module Components.Editor.EmbedLink.View
    exposing
        ( ViewModel
        , view
        )

import Html exposing (Html, span, div, text, input, button)
import Html.Attributes exposing (id, type_, value, readonly, disabled)
import Html.Events exposing (onFocus, onClick)
import Components.Editor.EmbedLink.Classes exposing (..)
import Shared.Constants as Constants
import Shared.Icons as Icons


autoselect : Html.Attribute msg
autoselect =
    Html.Attributes.attribute "onmouseup" "this.blur();this.select();"


type alias ViewModel msg =
    { projectId : String
    , revisionNumber : Int
    , onGist : msg
    , gistButtonEnabled : Bool
    }


directLink : ViewModel msg -> String
directLink { projectId, revisionNumber } =
    Constants.editorBase
        ++ "/"
        ++ projectId
        ++ "/"
        ++ toString revisionNumber


iframe : ViewModel msg -> String
iframe viewModel =
    "<iframe src=\""
        ++ embedLink viewModel
        ++ "\" style=\"width:100%; height:400px; border:0; border-radius: 3px; overflow:hidden;\""
        ++ " sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\""
        ++ "></iframe>"


embedLink : ViewModel msg -> String
embedLink { projectId, revisionNumber } =
    Constants.embedBase
        ++ "/"
        ++ projectId
        ++ "/"
        ++ toString revisionNumber


view : ViewModel msg -> Html msg
view viewModel =
    div [ class [ Container ] ]
        [ div [ class [ Links ] ]
            [ div [ class [ Link ] ]
                [ div
                    [ class [ LinkTitle ] ]
                    [ text "Direct Link (Medium, embed.ly)" ]
                , input
                    [ type_ "text"
                    , value <| directLink viewModel
                    , readonly True
                    , id "embedly_link"
                    , class [ LinkContent ]
                    , autoselect
                    ]
                    []
                ]
            , div [ class [ Link ] ]
                [ div
                    [ class [ LinkTitle ] ]
                    [ text "Embed Link" ]
                , input
                    [ type_ "text"
                    , value <| embedLink viewModel
                    , readonly True
                    , id "direct_link"
                    , class [ LinkContent ]
                    , autoselect
                    ]
                    []
                ]
            , div [ class [ Link ] ]
                [ div
                    [ class [ LinkTitle ] ]
                    [ text "IFrame" ]
                , input
                    [ id "iframe_link"
                    , value <| iframe viewModel
                    , type_ "text"
                    , class [ LinkContent ]
                    , autoselect
                    ]
                    []
                ]
            ]
        , div [ class [ Buttons ] ]
            [ button
                [ class [ Button ]
                , disabled (not viewModel.gistButtonEnabled)
                , onClick viewModel.onGist
                ]
                [ div [ class [ ButtonInner ] ]
                    [ span [ class [ ButtonIcon ] ] [ Icons.gitHub ]
                    , span [ class [ ButtonText ] ] [ text "Create Gist" ]
                    ]
                ]
            ]
        ]
