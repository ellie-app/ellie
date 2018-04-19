port module Pages.Embed.Effects.State
    exposing
        ( Config
        , Msg(..)
        , State
        , init
        , subscriptions
        , update
        )

import Network.Absinthe.Subscription as Subscription
import Pages.Embed.Effects.Handlers as Handlers
import Pages.Embed.Effects.Inbound as Inbound exposing (Inbound(..))
import Pages.Embed.Effects.Outbound as Outbound exposing (Outbound(..))
import Pages.Embed.Types.EmbedUpdate as EmbedUpdate exposing (EmbedUpdate)
import Pages.Embed.Types.RevisionId as RevisionId exposing (RevisionId)
import Time


processOutbound : Outbound msg -> State model -> ( State model, Cmd (Msg msg) )
processOutbound effect state =
    case effect of
        GetRevision id callback ->
            ( state
            , Handlers.getRevision id
                |> Cmd.map (callback >> UserMsg)
            )

        RunEmbed revisionId callback ->
            ( state
            , Handlers.runEmbed revisionId
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
        Inbound.EmbedUpdates revisionId callback ->
            case state.socket of
                Nothing ->
                    Time.every 100 (\_ -> SetupSocket revisionId)

                Just socket ->
                    Sub.map
                        (\info ->
                            case info of
                                Subscription.Data data ->
                                    UserMsg (callback data)

                                Subscription.Control msg ->
                                    SubscriptionMsg msg

                                Subscription.Status True ->
                                    UserMsg (callback EmbedUpdate.Connected)

                                Subscription.Status False ->
                                    UserMsg (callback EmbedUpdate.Disconnected)
                        )
                        (Subscription.listen socket)

        Inbound.Batch inbounds ->
            Sub.batch <| List.map (processInbound state) inbounds

        Inbound.None ->
            Sub.none



----


type Msg msg
    = UserMsg msg
    | SetupSocket RevisionId
    | SubscriptionMsg Subscription.Msg
    | NoOp


type alias State model =
    { socket : Maybe (Subscription.State EmbedUpdate)
    , model : model
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
            { socket = Nothing
            , model = model
            }

        ( state, cmd ) =
            processOutbound outbound initialState
    in
    ( state, cmd )


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

        SetupSocket revisionId ->
            ( { state | socket = Just <| Handlers.socket revisionId }
            , Cmd.none
            )

        SubscriptionMsg subMsg ->
            state.socket
                |> Maybe.map (Subscription.update subMsg)
                |> Maybe.map (Tuple.mapFirst (\s -> { state | socket = Just s }))
                |> Maybe.map (Tuple.mapSecond (Cmd.map SubscriptionMsg))
                |> Maybe.withDefault ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )
