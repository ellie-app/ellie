module Pages.Editor.Types.Settings
    exposing
        ( Settings
        , Theme(..)
        )


type Theme
    = Dark
    | Light


type alias Settings =
    { fontSize : String
    , fontFamily : String
    , theme : Theme
    , vimMode : Bool
    }
