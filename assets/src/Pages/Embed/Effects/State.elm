port module Pages.Embed.Effects.State
    exposing
        ( Config
        , Msg(..)
        , State
        , init
        , subscriptions
        , update
        )

import Pages.Embed.Effects.Handlers as Handlers
import Pages.Embed.Effects.Inbound as Inbound exposing (Inbound(..))
import Pages.Embed.Effects.Outbound as Outbound exposing (Outbound(..))


processOutbound : Outbound msg -> State model -> ( State model, Cmd (Msg msg) )
processOutbound effect state =
    case effect of
        GetRevision id callback ->
            ( state
            , Handlers.getRevision id
                |> Cmd.map (callback >> UserMsg)
            )

        Outbound.Batch outbounds ->
            outbounds
                |> List.foldr
                    (\outbound ( state, cmds ) ->
                        let
                            ( nextState, cmd ) =
                                processOutbound outbound state
                        in
                        ( nextState, cmd :: cmds )
                    )
                    ( state, [] )
                |> Tuple.mapSecond Cmd.batch

        Outbound.None ->
            ( state, Cmd.none )


processInbound : State model -> Inbound msg -> Sub (Msg msg)
processInbound state inbound =
    case inbound of
        Inbound.Batch inbounds ->
            Sub.batch <| List.map (processInbound state) inbounds

        Inbound.None ->
            Sub.none



----


type Msg msg
    = UserMsg msg
    | NoOp


type alias State model =
    { model : model
    }


type alias Config msg flags route model =
    { userUpdate : msg -> model -> ( model, Outbound msg )
    , userInit : flags -> route -> ( model, Outbound msg )
    , userSubs : model -> Inbound msg
    }


subscriptions : Config msg flags route model -> State model -> Sub (Msg msg)
subscriptions config state =
    processInbound state <| config.userSubs state.model


init :
    Config msg flags route model
    -> flags
    -> route
    -> ( State model, Cmd (Msg msg) )
init { userInit, userSubs } flags route =
    let
        ( model, outbound ) =
            userInit flags route

        initialState =
            { model = model
            }

        ( state, cmd ) =
            processOutbound outbound initialState
    in
    ( state
    , cmd
    )


update :
    Config msg flags route model
    -> Msg msg
    -> State model
    -> ( State model, Cmd (Msg msg) )
update ({ userUpdate } as config) msg state =
    case msg of
        UserMsg userMsg ->
            let
                ( nextModel, outbound ) =
                    userUpdate userMsg state.model

                ( nextState, cmd ) =
                    processOutbound outbound state
            in
            ( { nextState | model = nextModel }, cmd )

        NoOp ->
            ( state, Cmd.none )
