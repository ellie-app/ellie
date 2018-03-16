module Pages.Editor.Effects.State exposing (..)

import Debounce exposing (Debounce)
import Ellie.Types.User exposing (User)
import Pages.Editor.Effects.Exception exposing (Exception)
import Process


type Msg msg
    = UserMsg msg
    | DebouncePackageSearch Debounce.Msg
    | SaveSettingsSpawned Process.Id
    | DebounceSaveSettings Debounce.Msg
    | ExceptionOccured Exception
    | Multiple (List (Msg msg))
    | NoOp


type alias State msg =
    { debouncePackageSearch : Debounce (Cmd (Msg msg))
    , getUserTagger : Maybe (User -> msg)
    , saveSettingsId : Maybe Process.Id
    , debounceSaveSettings : Debounce (Cmd (Msg msg))
    }


init : State msg
init =
    { debouncePackageSearch = Debounce.init
    , getUserTagger = Nothing
    , saveSettingsId = Nothing
    , debounceSaveSettings = Debounce.init
    }
