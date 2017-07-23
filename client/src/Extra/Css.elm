module Extra.Css exposing (backdropFilter, blur)

import Css exposing (..)


type Filter units
    = Blur (ExplicitLength units)


blur : ExplicitLength units -> Filter units
blur =
    Blur


backdropFilter : Filter units -> Style
backdropFilter filter =
    let
        filterValue =
            case filter of
                Blur length ->
                    "blur(" ++ length.value ++ ")"
    in
    property "backdrop-filter" filterValue
