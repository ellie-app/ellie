module Pages.Embed.Model
    exposing
        ( Model
        , Tab(..)
        , model
        )

import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Revision as Revision exposing (Revision)
import Pages.Embed.Routing as Routing exposing (Route(..))
import RemoteData exposing (RemoteData(..))


type Tab
    = ElmTab
    | HtmlTab
    | ResultsTab


type alias Model =
    { currentRoute : Route
    , tab : Tab
    , revision : RemoteData ApiError Revision
    }


model : Model
model =
    { currentRoute = NotFound
    , tab = ElmTab
    , revision = NotAsked
    }
