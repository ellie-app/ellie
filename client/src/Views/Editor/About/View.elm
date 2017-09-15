module Views.Editor.About.View
    exposing
        ( CssClasses(..)
        , namespace
        , view
        )

import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Html exposing (Html, a, button, div, img, p, span, text)
import Html.Attributes exposing (href, id, src, style, target)
import Html.CssHelpers


type alias Config =
    { latestTermsVersion : TermsVersion }


view : Config -> Html msg
view config =
    div [ class [ Popout ] ]
        [ div [ class [ Title ] ]
            [ text "Ellie is the Elm platform in your browser."
            ]
        , p [ class [ Paragraph ] ]
            [ text "With Ellie you can use all of Elm’s features to build amazing animations, precise SSCCEs, cool demos, and anything else you could create with Elm in an ordinary development environment."
            ]
        , p [ class [ Paragraph ] ]
            [ text "Add a package if you need it in the sidebar, write a program, work through compiler errors, and share your work with the world."
            ]
        , p [ class [ Paragraph ] ]
            [ text "All content created with Ellie is released under the "
            , a [ class [ Link ], href "https://opensource.org/licenses/MIT" ] [ text "MIT license" ]
            , text ". We reserve the right to remove or modify any content created with Ellie for any reason. Report abuse, ask questions, or direct comments to "
            , a [ class [ Link ], href "mailto:ellie-app@lukewestby.com" ] [ text "ellie-app@lukewestby.com" ]
            , text ". Our latest terms of service can be found "
            , a [ class [ Link ], href <| TermsVersion.link config.latestTermsVersion, target "_blank" ] [ text "here" ]
            , text "."
            ]
        , p [ class [ Paragraph ] ]
            [ text "Copyright 2017 © Luke Westby" ]
        ]


namespace : String
namespace =
    "Views-Editor-About-"


type CssClasses
    = Popout
    | Title
    | Paragraph
    | Link


{ class, classList } =
    Html.CssHelpers.withNamespace namespace
