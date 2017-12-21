module Colors exposing (..)

import Css exposing (..)


type alias BoxShadow =
    { right : Style
    , bottom : Style
    , left : Style
    , bottomHover : Style
    , popout : Style
    , top : Style
    }


boxShadow : BoxShadow
boxShadow =
    { right = boxShadow5 (px 2) zero (px 4) zero <| rgba 0 0 0 0.75
    , bottom = boxShadow5 zero (px 2) (px 4) zero <| rgba 0 0 0 0.75
    , left = boxShadow5 (px -2) zero (px 4) zero <| rgba 0 0 0 0.75
    , bottomHover = boxShadow5 zero (px 3) (px 4) zero <| rgba 0 0 0 0.75
    , popout = boxShadow5 zero (px 2) (px 8) (px 2) <| rgba 0 0 0 0.75
    , top = boxShadow5 zero (px -2) (px 4) zero <| rgba 0 0 0 0.75
    }


darkMediumGray : Color
darkMediumGray =
    hex "292929"


lightGray : Color
lightGray =
    hex "#DDDDDD"


mediumGray : Color
mediumGray =
    hex "#525252"


pink : Color
pink =
    hex "#FC6ECC"


darkGray : Color
darkGray =
    hex "#1D1D1D"


lightMediumGray : Color
lightMediumGray =
    hex "#9C9C9C"


yellow : Color
yellow =
    hex "#decb6b"


green : Color
green =
    hex "#689f8e"


blue : Color
blue =
    hex "#82B1ff"


red : Color
red =
    hex "#ec5f67"
