module Apps.Embed.Model
    exposing
        ( Model
        , Tab(..)
        , model
        )

import RemoteData exposing (RemoteData(..))
import Apps.Embed.Routing as Routing exposing (Route(..))
import Data.Ellie.ApiError as ApiError exposing (ApiError)
import Data.Ellie.Revision as Revision exposing (Revision)


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
