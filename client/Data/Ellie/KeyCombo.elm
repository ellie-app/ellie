module Data.Ellie.KeyCombo exposing (..)

import Keyboard


type alias KeyCombo =
    { control : Bool
    , shift : Bool
    }


type Msg
    = ControlDown
    | ControlUp
    | ShiftDown
    | ShiftUp
    | NoOp


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Keyboard.downs
            (\code ->
                if code == 91 || code == 93 then
                    ControlDown
                else if code == 16 then
                    ShiftDown
                else
                    NoOp
            )
        , Keyboard.ups
            (\code ->
                if code == 91 || code == 93 then
                    ControlUp
                else if code == 16 then
                    ShiftUp
                else
                    NoOp
            )
        ]


update : Msg -> KeyCombo -> KeyCombo
update msg keyCombo =
    case msg of
        ControlDown ->
            { keyCombo | control = True }

        ControlUp ->
            { keyCombo | control = False }

        ShiftDown ->
            { keyCombo | shift = True }

        ShiftUp ->
            { keyCombo | shift = False }

        NoOp ->
            keyCombo


empty : KeyCombo
empty =
    { control = False
    , shift = False
    }


controlShift : KeyCombo -> Bool
controlShift { control, shift } =
    control && shift
