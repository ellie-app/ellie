module Pages.Editor.Types.User exposing (..)

import Data.Uuid as Uuid exposing (Uuid)
import Pages.Editor.Types.Settings as Settings exposing (Settings)


type alias User =
    { id : Uuid
    , settings : Settings
    , acceptedTerms : Maybe Int
    }
