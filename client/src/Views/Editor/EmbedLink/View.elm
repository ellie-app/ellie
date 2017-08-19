module Views.Editor.EmbedLink.View
    exposing
        ( Config
        , CssClasses(..)
        , namespace
        , view
        )

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (disabled, id, readonly, type_, value)
import Html.CssHelpers
import Html.Events exposing (onClick, onFocus)
import Shared.Constants as Constants
import Shared.Icons as Icons


autoselect : Html.Attribute msg
autoselect =
    Html.Attributes.attribute "onmouseup" "this.blur();this.select();"


type alias Config msg =
    { projectId : String
    , revisionNumber : Int
    , onGist : msg
    , gistButtonEnabled : Bool
    }


directLink : Config msg -> String
directLink { projectId, revisionNumber } =
    Constants.editorBase
        ++ "/"
        ++ projectId
        ++ "/"
        ++ toString revisionNumber


iframe : Config msg -> String
iframe config =
    "<iframe src=\""
        ++ embedLink config
        ++ "\" style=\"width:100%; height:400px; border:0; border-radius: 3px; overflow:hidden;\""
        ++ " sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\""
        ++ "></iframe>"


embedLink : Config msg -> String
embedLink { projectId, revisionNumber } =
    Constants.embedBase
        ++ "/"
        ++ projectId
        ++ "/"
        ++ toString revisionNumber


view : Config msg -> Html msg
view config =
    div [ class [ Container ] ]
        [ div [ class [ Links ] ]
            [ div [ class [ Link ] ]
                [ div
                    [ class [ LinkTitle ] ]
                    [ text "Direct Link (Medium, embed.ly)" ]
                , input
                    [ type_ "text"
                    , value <| directLink config
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
                    , value <| embedLink config
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
                    , value <| iframe config
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
                , disabled (not config.gistButtonEnabled)
                , onClick config.onGist
                ]
                [ div [ class [ ButtonInner ] ]
                    [ span [ class [ ButtonIcon ] ] [ Icons.gitHub ]
                    , span [ class [ ButtonText ] ] [ text "Create Gist" ]
                    ]
                ]
            ]
        ]


type CssClasses
    = Container
    | Links
    | Link
    | LinkTitle
    | LinkContent
    | Buttons
    | Button
    | ButtonInner
    | ButtonText
    | ButtonIcon


namespace : String
namespace =
    "Views-Editor-EmbedLink-"


{ class, classList } =
    Html.CssHelpers.withNamespace namespace
