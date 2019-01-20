module Pages.Editor.Views.Workbench.Share exposing (Config, container, heading, iframe, preview, view, warning)

import Css exposing (..)
import Data.Url as Url
import Ellie.Ui.Button as Button
import Ellie.Ui.CopyText as CopyText
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Pages.Editor.Types.Revision as Revision exposing (Revision)


type alias Config msg =
    { onDownloadZip : msg
    , revisionId : Revision.Id
    , hasUnsavedChanges : Bool
    }


view : Config msg -> Html msg
view config =
    Html.styled Html.div
        [ position absolute
        , top zero
        , left zero
        , width (pct 100)
        , height (pct 100)
        , backgroundColor Theme.secondaryBackground
        , padding (px 16)
        ]
        []
        [ if config.hasUnsavedChanges then
            container [ warning ]

          else
            Html.text ""
        , container
            [ heading "Export"
            , Html.div
                [ css [ displayFlex ] ]
                [ Button.view
                    { icon = Just Icon.Zip
                    , label = "Download Zip"
                    , action = Button.click config.onDownloadZip
                    }
                ]
            ]
        , container
            [ heading "Share on Medium (Embed.ly)"
            , CopyText.view <| Url.toString <| Revision.editorLink config.revisionId
            ]
        , container
            [ heading "Embed IFrame"
            , CopyText.view (iframe config.revisionId)
            ]
        , container
            [ heading "Preview"
            , preview config.revisionId
            ]
        ]


warning : Html msg
warning =
    Html.styled Html.div
        [ padding (px 12)
        , backgroundColor Theme.primaryBackground
        , displayFlex
        , alignItems center
        , justifyContent flexStart
        ]
        []
        [ Html.styled Html.div
            [ color Theme.warning
            , width (px 20)
            , height (px 20)
            , marginRight (px 16)
            ]
            []
            [ Icon.view Icon.Warning ]
        , Html.styled Html.div
            [ color Theme.primaryForeground
            , fontSize (px 16)
            ]
            []
            [ Html.text "You have unsaved changes" ]
        ]


iframe : Revision.Id -> String
iframe revisionId =
    "<iframe src=\"" ++ Url.toString (Revision.embedLink revisionId) ++ "\" style=\"width:100%; height:400px; border:0; overflow:hidden;\" sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\"></iframe>"


preview : Revision.Id -> Html msg
preview revisionId =
    Html.styled Html.iframe
        [ width (pct 100)
        , height (px 500)
        , border3 (px 2) solid Theme.draggableBorder
        , position relative
        , zIndex (int 1)
        ]
        [ Url.src <| Revision.embedLink revisionId ]
        []


container : List (Html msg) -> Html msg
container =
    Html.styled Html.div
        [ paddingBottom (px 16) ]
        []


heading : String -> Html msg
heading content =
    Html.styled Html.div
        [ fontSize (px 14)
        , color Theme.primaryForeground
        , fontWeight bold
        , lineHeight (num 1)
        , paddingBottom (px 8)
        , textTransform uppercase
        ]
        []
        [ Html.text content ]
