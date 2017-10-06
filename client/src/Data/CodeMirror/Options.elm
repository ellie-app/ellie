module Data.CodeMirror.Options exposing (Options, default, encoder)

import Json.Encode as Encode exposing (Value)


type alias Options =
    { vimMode : Bool
    , theme : String
    , mode : String
    , initialValue : String
    , readOnly : Bool
    , tabSize : Int
    }


default : Options
default =
    { vimMode = False
    , theme = "material"
    , mode = "elm"
    , initialValue = ""
    , readOnly = False
    , tabSize = 4
    }


encoder : Options -> Value
encoder options =
    Encode.object
        [ ( "vimMode", Encode.bool options.vimMode )
        , ( "theme", Encode.string options.theme )
        , ( "mode", Encode.string options.mode )
        , ( "initialValue", Encode.string options.initialValue )
        , ( "readOnly", Encode.bool options.readOnly )
        , ( "tabSize", Encode.int options.tabSize )
        ]
