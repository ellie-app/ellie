module Pages.Editor.Effects.State exposing (..)

import Debounce exposing (Debounce)
import Ellie.Types.User exposing (User)


type Msg msg
    = UserMsg msg
    | DebouncePackageSearch Debounce.Msg
    | NoOp


type alias State msg =
    { debouncePackageSearch : Debounce (Cmd (Msg msg))
    , getUserTagger : Maybe (User -> msg)
    }


init : State msg
init =
    { debouncePackageSearch = Debounce.init
    , getUserTagger = Nothing
    }
