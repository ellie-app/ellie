module Css.Extra exposing (backdropFilter, blur, filter, withAlpha)

import Css exposing (..)


type Filter units
    = Blur (ExplicitLength units)


blur : ExplicitLength units -> Filter units
blur =
    Blur


filterToString : Filter units -> String
filterToString filterValue =
    case filterValue of
        Blur length ->
            "blur(" ++ length.value ++ ")"


backdropFilter : Filter units -> Style
backdropFilter filterValue =
    filterValue
        |> filterToString
        |> property "backdrop-filter"


filter : Filter units -> Style
filter filterValue =
    filterValue
        |> filterToString
        |> property "filter"


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    rgba color.red color.green color.blue alpha
