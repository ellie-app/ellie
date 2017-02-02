module Components.About.View
    exposing
        ( view
        )

import Html exposing (Html, a, p, div, text, button, span, img)
import Html.Attributes exposing (style, src, href, id, target)
import Shared.Icons as Icons
import Shared.Colors as Colors
import Components.About.Classes exposing (..)


view : Html msg
view =
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
            [ text "All content created with Ellie is released in the public domain "
            , a [ class [ Link ], href "https://creativecommons.org/share-your-work/public-domain/cc0/" ] [ text "(CC0)" ]
            , text ". HumbleSpark LLC reserves the right to remove or modify any content created with Ellie for any reason. Report abuse, ask questions, or direct comments to "
            , a [ class [ Link ], href "mailto:ellie-app@humblespark.com" ] [ text "ellie-app@humblespark.com" ]
            ]
        , div [ class [ Creators ] ]
            [ div [ class [ CreatorLine ] ]
                [ span [ class [ Logo ] ]
                    [ text "Ellie" ]
                , span []
                    [ text " was created for the Elm community by" ]
                ]
            , div [ class [ ImagesAndHsLogo ] ]
                [ a
                    [ class [ PartnerImageContainer ]
                    , href "https://twitter.com/luke_dot_js"
                    , target "_blank"
                    ]
                    [ img
                        [ src "/images/luke_profile.jpg"
                        , class [ PartnerImage ]
                        ]
                        []
                    , text "Luke"
                    ]
                , span [] [ text "&" ]
                , a
                    [ class [ PartnerImageContainer ]
                    , href "https://twitter.com/nickdreckshage"
                    , target "_blank"
                    ]
                    [ img
                        [ src "/images/nick_profile.jpg"
                        , class [ PartnerImage ]
                        ]
                        []
                    , text "Nick"
                    ]
                , span [] [ text "at" ]
                , a
                    [ class [ HsLogoImage ]
                    , href "https://humblespark.com"
                    , target "_blank"
                    ]
                    []
                ]
            ]
        ]
