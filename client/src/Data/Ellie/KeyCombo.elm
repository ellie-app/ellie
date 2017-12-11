module Data.Ellie.KeyCombo exposing (..)

import Keyboard
import Set exposing (Set)


type alias KeyCombo =
    { control : Bool
    , shift : Bool
    }


controlKeyCodes : Set Int
controlKeyCodes =
    Set.fromList [ 91, 92, 93, 224 ]


controlKey : Int -> Bool
controlKey code =
    controlKeyCodes
        |> Set.member code


shiftKeyCodes : Set Int
shiftKeyCodes =
    Set.singleton 16


shiftKey : Int -> Bool
shiftKey code =
    shiftKeyCodes
        |> Set.member code


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
                if controlKey code then
                    ControlDown
                else if shiftKey code then
                    ShiftDown
                else
                    NoOp
            )
        , Keyboard.ups
            (\code ->
                if controlKey code then
                    ControlUp
                else if shiftKey code then
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
