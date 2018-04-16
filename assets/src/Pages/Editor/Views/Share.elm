module Pages.Editor.Views.Share exposing (..)

import Css exposing (..)
import Data.Url as Url
import Ellie.Ui.Button as Button
import Ellie.Ui.CopyText as CopyText
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Pages.Editor.Types.RevisionId as RevisionId exposing (RevisionId)


type alias Config msg =
    { onCreateGist : msg
    , onDownloadZip : msg
    , revisionId : RevisionId
    }


view : Config msg -> Html msg
view config =
    Html.div
        [ css
            [ position absolute
            , top zero
            , left zero
            , width (pct 100)
            , height (pct 100)
            , backgroundColor Theme.secondaryBackground
            , padding (px 16)
            ]
        ]
        [ Html.div [ containerStyles ]
            [ Html.div [ headerStyles ]
                [ Html.text "Export" ]
            , Html.div
                [ css [ displayFlex ] ]
                [ Html.div
                    [ css [ paddingRight (px 16) ] ]
                    [ Button.view
                        { icon = Just Icon.GitHub
                        , label = "Create Gist"
                        , action = Button.click config.onCreateGist
                        }
                    ]
                , Button.view
                    { icon = Just Icon.Zip
                    , label = "Download Zip"
                    , action = Button.click config.onDownloadZip
                    }
                ]
            ]
        , Html.div [ containerStyles ]
            [ Html.div [ headerStyles ] [ Html.text "Share on Medium (Embed.ly)" ]
            , CopyText.view <| Url.toString <| RevisionId.editorLink config.revisionId
            ]
        , Html.div [ containerStyles ]
            [ Html.div [ headerStyles ] [ Html.text "Embed IFrame" ]
            , CopyText.view (iframe config.revisionId)
            ]
        ]


iframe : RevisionId -> String
iframe revisionId =
    "<iframe src=\"" ++ Url.toString (RevisionId.embedLink revisionId) ++ "\" style=\"width:100%; height:400px; border:0; overflow:hidden;\" sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\"></iframe>"


containerStyles : Attribute msg
containerStyles =
    css
        [ paddingBottom (px 16)
        ]


headerStyles : Attribute msg
headerStyles =
    css
        [ fontSize (px 14)
        , color Theme.primaryForeground
        , fontWeight bold
        , lineHeight (num 1)
        , paddingBottom (px 8)
        , textTransform uppercase
        ]
