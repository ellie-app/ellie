module Views.Editor.Terms.View exposing (Config, CssClasses(..), namespace, view)

import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Html exposing (Html, a, button, div, h1, iframe, input, option, select, span, text)
import Html.Attributes as Attr exposing (href, id, placeholder, selected, src, target, type_, value)
import Html.CssHelpers
import Views.Button.View as Button


namespace : String
namespace =
    "Views-Editor-Terms-"


type alias Config msg =
    { onAccept : msg
    , termsVersion : TermsVersion
    }


view : Config msg -> Html msg
view config =
    div [ class [ Popout ] ]
        [ div [ class [ Content ] ]
            [ text "Before saving, please accept our "
            , a
                [ href <| TermsVersion.link config.termsVersion
                , target "_blank"
                , class [ Link ]
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


type CssClasses
    = Popout
    | Content
    | Link


{ class } =
    Html.CssHelpers.withNamespace namespace
