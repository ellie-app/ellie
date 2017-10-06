module Pages.Editor.Sidebar.Model exposing (..)

import Data.Elm.Package as Package exposing (Package)


type Panel
    = Settings
    | About
    | Packages


type alias Model =
    { search : String
    , results : List Package
    , panel : Panel
    }


model : Model
model =
    { search = ""
    , results = []
    , panel = Packages
    }


resetSearch : Model -> Model
resetSearch model =
    { model
        | search = ""
        , results = []
    }
