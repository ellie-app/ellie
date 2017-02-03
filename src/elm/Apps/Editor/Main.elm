module Apps.Editor.Main exposing (main)

import Navigation
import Apps.Editor.Model as Model exposing (Model, Flags)
import Apps.Editor.Update as Update exposing (Msg(..))
import Apps.Editor.View as View
import Apps.Editor.Subs as Subs
import Apps.Editor.Cmds as Cmds


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


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Update.onRouteChange
        { view = View.view
        , init = Update.initialize
        , subscriptions = Subs.subscriptions
        , update =
            wrapUpdate
                Update.update
                [ (Model.hasUnsavedWork >> Cmds.hasUnsavedWork) ]
        }
