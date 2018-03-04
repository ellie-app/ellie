module Pages.Editor.Views.Share exposing (..)

import Css exposing (..)
import Ellie.Ui.Button as Button
import Ellie.Ui.CopyText as CopyText
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)


type alias Config msg =
    { onCreateGist : msg
    , onDownloadZip : msg
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
                        , disabled = False
                        , action = Button.click config.onCreateGist
                        }
                    ]
                , Button.view
                    { icon = Just Icon.Zip
                    , label = "Download Zip"
                    , disabled = False
                    , action = Button.click config.onDownloadZip
                    }
                ]
            ]
        , Html.div [ containerStyles ]
            [ Html.div [ headerStyles ] [ Html.text "Share on Medium (Embed.ly)" ]
            , CopyText.view "http://localhost:1337/lol"
            ]
        , Html.div [ containerStyles ]
            [ Html.div [ headerStyles ] [ Html.text "Embed IFrame" ]
            , CopyText.view iframe
            ]
        ]


iframe : String
iframe =
    """<iframe src="https://ellie-app.com/embed/nywq2JvGQa1/1" style="width:100%; height:400px; border:0; overflow:hidden;" sandbox="allow-modals allow-forms allow-popups allow-scripts allow-same-origin"></iframe>"""


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
