module Pages.Editor.Header.Model exposing (..)


type alias Model =
    { shareOpen : Bool }


init : Model
init =
    { shareOpen = False }
