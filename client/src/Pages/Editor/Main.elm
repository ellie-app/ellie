module Pages.Editor.Main exposing (main)

import Navigation
import Pages.Editor.Cmds as Cmds
import Pages.Editor.Model as Model exposing (Flags, Model)
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


main : Program Flags Model Msg
main =
    Navigation.programWithFlags Update.onRouteChange
        { view = View.view
        , init = Update.initialize
        , subscriptions = Subs.subscriptions
        , update =
            wrapUpdate
                Update.update
                [ Model.hasUnsavedWork >> Cmds.hasUnsavedWork ]
        }
