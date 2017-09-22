module Views.Editor.Terms exposing (Config, view)

import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Html exposing (Html, a, button, div, h1, iframe, input, option, select, span, text)
import Html.Attributes as Attr exposing (href, id, placeholder, selected, src, target, type_, value)
import Views.Button as Button
import Views.Editor.Terms.Styles as Styles


type alias Config msg =
    { onAccept : msg
    , termsVersion : TermsVersion
    }


view : Config msg -> Html msg
view config =
    div [ Styles.popout ]
        [ div [ Styles.content ]
            [ text "Before saving, please accept our "
            , a
                [ href <| TermsVersion.link config.termsVersion
                , target "_blank"
                , Styles.link
                ]
                [ text "Terms of Service" ]
            ]
        , Button.view
            { onClick = config.onAccept
            , disabled = False
            , label = "Accept and Save"
            , width = Nothing
            }
        ]
