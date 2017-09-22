module Colors exposing (..)


white : String
white =
    "#e9eded"


pink : String
pink =
    "#ff6cc9"


purple : String
purple =
    "#c58eff"


orange : String
orange =
    "#f77669"


yellow : String
yellow =
    "#decb6b"


green : String
green =
    "#689f8e"


blue : String
blue =
    "#82B1ff"


red : String
red =
    "#ec5f67"


darkGray : String
darkGray =
    "#282a37"


mediumGrayRgb : { r : Int, g : Int, b : Int }
mediumGrayRgb =
    { r = 86, g = 88, b = 104 }


darkGrayRgb : { r : Int, g : Int, b : Int }
darkGrayRgb =
    { r = 40, g = 42, b = 55 }


mediumGray : String
mediumGray =
    "#565868"


lightGray : String
lightGray =
    "#8D8D97"


lightGrayRgb : { r : Int, g : Int, b : Int }
lightGrayRgb =
    { r = 141, g = 141, b = 151 }


pinkPurpleGradient : String
pinkPurpleGradient =
    "linear-gradient(134deg, " ++ pink ++ " 0%, " ++ purple ++ " 100%)"
