module Views.Editor.About exposing (Config, view)

import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Html exposing (Html, a, button, div, img, p, span, text)
import Html.Attributes exposing (href, id, src, style, target)
import Views.Editor.About.Styles as Styles


type alias Config =
    { latestTermsVersion : TermsVersion }


view : Config -> Html msg
view config =
    div [ Styles.popout ]
        [ div [ Styles.title ]
            [ text "Ellie is the Elm platform in your browser."
            ]
        , p [ Styles.paragraph ]
            [ text "With Ellie you can use all of Elm’s features to build amazing animations, precise SSCCEs, cool demos, and anything else you could create with Elm in an ordinary development environment."
            ]
        , p [ Styles.paragraph ]
            [ text "Add a package if you need it in the sidebar, write a program, work through compiler errors, and share your work with the world."
            ]
        , p [ Styles.paragraph ]
            [ text "All content created with Ellie is released under the "
            , a [ Styles.link, href "https://opensource.org/licenses/MIT" ] [ text "MIT license" ]
            , text ". We reserve the right to remove or modify any content created with Ellie for any reason. Report abuse, ask questions, or direct comments to "
            , a [ Styles.link, href "mailto:ellie-app@lukewestby.com" ] [ text "ellie-app@lukewestby.com" ]
            , text ". Our latest terms of service can be found "
            , a [ Styles.link, href <| TermsVersion.link config.latestTermsVersion, target "_blank" ] [ text "here" ]
            , text "."
            ]
        , p [ Styles.paragraph ]
            [ text "Copyright 2017 © Luke Westby" ]
        ]
