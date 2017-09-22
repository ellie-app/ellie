module Views.Editor.EmbedLink exposing (Config, view)

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (disabled, id, readonly, type_, value)
import Html.Events exposing (onClick, onFocus)
import Shared.Constants as Constants
import Shared.Icons as Icons
import Views.Editor.EmbedLink.Styles as Styles


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
    div [ Styles.container ]
        [ div []
            [ div [ Styles.link ]
                [ div
                    [ Styles.linkTitle ]
                    [ text "Direct Link (Medium, embed.ly)" ]
                , input
                    [ type_ "text"
                    , value <| directLink config
                    , readonly True
                    , id "embedly_link"
                    , Styles.linkContent
                    , autoselect
                    ]
                    []
                ]
            , div [ Styles.link ]
                [ div
                    [ Styles.linkTitle ]
                    [ text "Embed Link" ]
                , input
                    [ type_ "text"
                    , value <| embedLink config
                    , readonly True
                    , id "direct_link"
                    , Styles.linkContent
                    , autoselect
                    ]
                    []
                ]
            , div [ Styles.link ]
                [ div
                    [ Styles.linkTitle ]
                    [ text "IFrame" ]
                , input
                    [ id "iframe_link"
                    , value <| iframe config
                    , type_ "text"
                    , Styles.linkContent
                    , autoselect
                    ]
                    []
                ]
            ]
        , div [ Styles.buttons ]
            [ button
                [ Styles.button
                , disabled (not config.gistButtonEnabled)
                , onClick config.onGist
                ]
                [ div [ Styles.buttonInner ]
                    [ span [ Styles.buttonIcon ] [ Icons.gitHub ]
                    , span [] [ text "Create Gist" ]
                    ]
                ]
            ]
        ]
