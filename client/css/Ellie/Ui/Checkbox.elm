module Ellie.Ui.Checkbox exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


actualInput =
    uniqueClass
        [ display none
        ]


container =
    uniqueClass
        [ displayFlex
        , cursor pointer
        , alignItems center
        , hover
            [ descendants
                [ selector "[data-checkbox]"
                    [ fill Colors.lightMediumGray
                    ]
                ]
            ]
        , active
            [ descendants
                [ selector "[data-checkbox]"
                    [ fill Colors.mediumGray_ ]
                ]
            ]
        ]


label =
    uniqueClass
        [ cursor pointer
        , marginLeft (px 8)
        ]


svgCheckbox =
    uniqueSvgClass
        [ width (px 16)
        , height (px 16)
        , fill Colors.mediumGray_
        ]
