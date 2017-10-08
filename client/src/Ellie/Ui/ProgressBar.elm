module Ellie.Ui.ProgressBar exposing (..)

import Ellie.Ui.ProgressBar.Styles as Styles
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type Progress
    = Indeterminate
    | Percentage Float


viewIndeterminate : Html msg
viewIndeterminate =
    div [ Styles.outer ]
        [ div [ Styles.innerIndeterminate ] []
        ]


view : Progress -> Html msg
view progress =
    case progress of
        Indeterminate ->
            viewIndeterminate

        Percentage percentage ->
            div [ Styles.outer ]
                [ div
                    [ Styles.innerPercentage
                    , style
                        [ ( "transform"
                          , "scaleX(" ++ toString percentage ++ ")"
                          )
                        ]
                    ]
                    []
                ]
