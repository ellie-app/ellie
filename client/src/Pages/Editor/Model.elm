module Pages.Editor.Model
    exposing
        ( Model
        , model
        , workspace_
        )

import Data.Entity as Entity exposing (Entity)
import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.Workspace as Workspace exposing (Workspace)
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Header.Model as Header
import Pages.Editor.Layout.Model as Layout
import Pages.Editor.Logs.Model as Logs
import Pages.Editor.Route as Route
import Pages.Editor.Sidebar.Model as Sidebar


type alias Model =
    { workspace : Maybe Workspace
    , revision : Maybe (Entity Revision.Id Revision)
    , token : Maybe Jwt
    , route : Route.Route
    , layout : Layout.Model
    , sidebar : Sidebar.Model
    , header : Header.Model
    , logs : Logs.Model
    }


model : Flags -> Route.Route -> Model
model flags route =
    { workspace = Nothing
    , revision = Nothing
    , token = flags.token
    , route = route
    , layout = Layout.model flags.windowSize
    , sidebar = Sidebar.model
    , header = Header.model
    , logs = Logs.model
    }


workspace_ : (Workspace -> Workspace) -> Model -> Model
workspace_ f ({ workspace } as model) =
    { model | workspace = Maybe.map f workspace }
