module Pages.Embed.State.App exposing (..)

import Data.Replaceable as Replaceable exposing (Replaceable)
import Json.Decode as Decode exposing (Decoder)
import Pages.Embed.Effects.Handlers exposing (GetRevisionError)
import Pages.Embed.Effects.Inbound as Inbound exposing (Inbound)
import Pages.Embed.Effects.Outbound as Outbound exposing (Outbound)
import Pages.Embed.Types.Panel as Panel exposing (Panel)
import Pages.Embed.Types.Revision as Revision exposing (Revision)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)
import Pages.Embed.Types.Route as Route exposing (Route)


type Model
    = Failure
    | Working
        { panel : Panel
        , revision : Replaceable RevisionId Revision
        }


type alias Flags =
    {}


flags : Decoder Flags
flags =
    Decode.succeed {}


init : Flags -> Route -> ( Model, Outbound Msg )
init flags route =
    case route of
        Route.NotFound ->
            ( Failure, Outbound.none )

        Route.Existing revisionId panel ->
            ( Working
                { panel = panel
                , revision = Replaceable.Loading revisionId
                }
            , Outbound.GetRevision revisionId (RevisionLoaded revisionId)
            )


type Msg
    = RevisionLoaded RevisionId (Result GetRevisionError Revision)
    | RouteChanged Route


update : Msg -> Model -> ( Model, Outbound Msg )
update msg model =
    case ( model, msg ) of
        ( _, RouteChanged route ) ->
            init {} route

        ( Failure, _ ) ->
            ( model, Outbound.none )

        ( Working state, RevisionLoaded revisionId (Ok revision) ) ->
            ( Working { state | revision = Replaceable.Loaded ( revisionId, revision ) }
            , Outbound.none
            )

        ( Working state, RevisionLoaded revisionId (Err error) ) ->
            ( Failure, Outbound.none )


subscriptions : Model -> Inbound msg
subscriptions model =
    Inbound.none
