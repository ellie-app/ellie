module Pages.Editor.Main exposing (main)

import Html
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Pages.Editor.Cmds as Cmds
import Pages.Editor.Flags as Flags exposing (Flags)
import Pages.Editor.Model as Model exposing (Model)
import Pages.Editor.Subs as Subs
import Pages.Editor.Update as Update exposing (Msg(..))
import Pages.Editor.View as View


wrapUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> List (model -> Cmd msg)
    -> (msg -> model -> ( model, Cmd msg ))
wrapUpdate update additionalCmdCreators =
    \msg model ->
        let
            ( nextModel, nextCmd ) =
                update msg model

            additionalCmds =
                List.map ((|>) nextModel) additionalCmdCreators
        in
        ( nextModel
        , Cmd.batch <| nextCmd :: additionalCmds
        )


init : Value -> Location -> ( Model, Cmd Msg )
init flagsJson location =
    case Decode.decodeValue Flags.decoder flagsJson of
        Ok flags ->
            Update.initialize flags location

        Err msg ->
            Debug.crash msg


main : Program Value Model Msg
main =
    Navigation.programWithFlags Update.onRouteChange
        { view = View.view
        , init = init
        , subscriptions = Subs.subscriptions
        , update =
            wrapUpdate
                Update.update
                [ Model.hasUnsavedWork >> Cmds.hasUnsavedWork ]
        }
