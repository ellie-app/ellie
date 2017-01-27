module Components.Editor.EmbedLink.View
    exposing
        ( ViewModel
        , view
        )

import Html exposing (Html, div, text)
import Components.Editor.EmbedLink.Classes exposing (..)
import Types.Revision as Revision exposing (Revision)
import Shared.Constants as Constants


type alias ViewModel =
    { projectId : String
    , revisionNumber : Int
    }


view : ViewModel -> Html msg
view { projectId, revisionNumber } =
    div [ class [ Container ] ]
        [ text <|
            "<iframe src=\""
                ++ Constants.embedBase
                ++ "/"
                ++ projectId
                ++ "/"
                ++ toString revisionNumber
                ++ "\""
                ++ " style=\"width:100%; height:400px; border:0; border-radius: 3px; overflow:hidden;\""
                ++ " sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\""
                ++ "></iframe>"
        ]
