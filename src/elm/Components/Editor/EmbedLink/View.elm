module Components.Editor.EmbedLink.View
    exposing
        ( ViewModel
        , view
        )

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (id, type_, value, readonly)
import Html.Events exposing (onFocus)
import Components.Editor.EmbedLink.Classes exposing (..)
import Types.Revision as Revision exposing (Revision)
import Shared.Constants as Constants


autoselect : Html.Attribute msg
autoselect =
    Html.Attributes.attribute "onmouseup" "this.blur();this.select();"


type alias ViewModel =
    { projectId : String
    , revisionNumber : Int
    }


embedlyLink : ViewModel -> String
embedlyLink { projectId, revisionNumber } =
    Constants.editorBase
        ++ "/"
        ++ projectId
        ++ "/"
        ++ toString revisionNumber


iframe : ViewModel -> String
iframe viewModel =
    "<iframe src=\""
        ++ directLink viewModel
        ++ "\" style=\"width:100%; height:400px; border:0; border-radius: 3px; overflow:hidden;\""
        ++ " sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\""
        ++ "></iframe>"


directLink : ViewModel -> String
directLink { projectId, revisionNumber } =
    Constants.embedBase
        ++ "/"
        ++ projectId
        ++ "/"
        ++ toString revisionNumber


view : ViewModel -> Html msg
view viewModel =
    div [ class [ Container ] ]
        [ div [ class [ Option ] ]
            [ div
                [ class [ OptionTitle ] ]
                [ text "Medium, embed.ly" ]
            , input
                [ type_ "text"
                , value <| embedlyLink viewModel
                , readonly True
                , id "embedly_link"
                , class [ OptionContent ]
                , autoselect
                ]
                []
            ]
        , div [ class [ Option ] ]
            [ div
                [ class [ OptionTitle ] ]
                [ text "Direct Link" ]
            , input
                [ type_ "text"
                , value <| directLink viewModel
                , readonly True
                , id "direct_link"
                , class [ OptionContent ]
                , autoselect
                ]
                []
            ]
        , div [ class [ Option ] ]
            [ div
                [ class [ OptionTitle ] ]
                [ text "IFrame" ]
            , input
                [ id "iframe_link"
                , value <| iframe viewModel
                , type_ "text"
                , class [ OptionContent ]
                , autoselect
                ]
                []
            ]
        ]
