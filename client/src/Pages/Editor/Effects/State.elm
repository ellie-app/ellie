module Pages.Editor.Effects.State exposing (..)

import Debounce exposing (Debounce)


type Msg msg
    = UserMsg msg
    | DebouncePackageSearch Debounce.Msg
    | NoOp


type alias State msg =
    { debouncePackageSearch : Debounce (Cmd (Msg msg))
    }


init : State msg
init =
    { debouncePackageSearch = Debounce.init
    }
