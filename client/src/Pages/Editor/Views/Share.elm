module Pages.Editor.Views.Share exposing (..)

import Css exposing (..)
import Ellie.Ui.Button as Button
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Html)
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
        [ Html.div []
            [ Html.div
                [ css
                    [ fontSize (px 14)
                    , color Theme.primaryForeground
                    , fontWeight bold
                    , lineHeight (num 1)
                    , paddingBottom (px 8)
                    , textTransform uppercase
                    ]
                ]
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
        ]
