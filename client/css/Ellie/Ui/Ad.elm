module Ellie.Ui.Ad exposing (..)

import Colors
import Css exposing (..)
import Css.File exposing (..)


container =
    uniqueClass
        [ Colors.boxShadow |> .top
        , zIndex (int 1)
        , position relative
        ]
