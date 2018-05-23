module Pages.Editor.Types.User exposing (..)

import Extra.Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias User =
    { settings : Settings
    , acceptedTerms : Maybe Int
    }


type Theme
    = Dark
    | Light


type alias Settings =
    { fontSize : String
    , fontFamily : String
    , theme : Theme
    , vimMode : Bool
    }


default : User
default =
    { acceptedTerms = Nothing
    , settings =
        { fontSize = "14px"
        , fontFamily = "monospace"
        , theme = Dark
        , vimMode = False
        }
    }


localStorageEncoder : User -> Value
localStorageEncoder user =
    Encode.object
        [ ( "acceptedTerms", Encode.maybeNull Encode.int user.acceptedTerms )
        , ( "settings"
          , Encode.object
                [ ( "fontSize", Encode.string user.settings.fontSize )
                , ( "fontFamily", Encode.string user.settings.fontFamily )
                , ( "vimMode", Encode.bool user.settings.vimMode )
                , ( "theme"
                  , case user.settings.theme of
                        Dark ->
                            Encode.string "DARK"

                        Light ->
                            Encode.string "LIGHT"
                  )
                ]
          )
        ]


localStorageDecoder : Decoder User
localStorageDecoder =
    let
        themeDecoder : Decoder Theme
        themeDecoder =
            Decode.andThen
                (\theme ->
                    case theme of
                        "DARK" ->
                            Decode.succeed Dark

                        "LIGHT" ->
                            Decode.succeed Light

                        _ ->
                            Decode.fail "Like who really run this? \\ Like who really run that man that say he run this? \\ Wh-who really run that man that say he run this, r-r-run run this?"
                )
                Decode.string

        settingsDecoder : Decoder Settings
        settingsDecoder =
            Decode.map4 Settings
                (Decode.field "fontSize" Decode.string)
                (Decode.field "fontFamily" Decode.string)
                (Decode.field "theme" themeDecoder)
                (Decode.field "vimMode" Decode.bool)
    in
    Decode.map2 User
        (Decode.field "settings" settingsDecoder)
        (Decode.field "acceptedTerms" (Decode.nullable Decode.int))
