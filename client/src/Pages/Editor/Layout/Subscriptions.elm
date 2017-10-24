port module Pages.Editor.Layout.Subscriptions exposing (subscriptions)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Mouse
import Pages.Editor.Layout.Model as Model exposing (DragTarget(..), Model)
import Pages.Editor.Layout.Update exposing (Msg(..))
import Window


port pagesEditorLayoutSubscriptionsIn : (Value -> msg) -> Sub msg


decodeMsg : Model -> Value -> Msg
decodeMsg model value =
    let
        decoder =
            Decode.field "type" Decode.string
                |> Decode.andThen
                    (\type_ ->
                        case type_ of
                            "WindowMessage" ->
                                model
                                    |> windowMessageDecoder
                                    |> Decode.at [ "args", "0" ]

                            _ ->
                                Decode.fail <| "Pages.Editor.Layout.Subscriptions: Unrecognized Msg constructor " ++ type_
                    )
    in
    case Decode.decodeValue decoder value of
        Ok msg ->
            msg

        Err message ->
            NoOp


windowMessageDecoder : Model -> Decoder Msg
windowMessageDecoder model =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "mouseup" ->
                        Decode.succeed <|
                            case model.dragTarget of
                                EditorsHandle ->
                                    EditorDragEnded

                                OutputHandle ->
                                    ResultDragEnded

                                LogsHandle ->
                                    LogsDragEnded

                                NoTarget ->
                                    NoOp

                    "mousemove" ->
                        Decode.decode Mouse.Position
                            |> Decode.required "x" Decode.int
                            |> Decode.required "y" Decode.int
                            |> Decode.map
                                (\position ->
                                    case model.dragTarget of
                                        EditorsHandle ->
                                            EditorDragged position

                                        OutputHandle ->
                                            ResultDragged position

                                        LogsHandle ->
                                            LogsDragged position

                                        NoTarget ->
                                            NoOp
                                )

                    _ ->
                        Decode.succeed NoOp
            )


drags : Model -> Sub Msg
drags model =
    case model.dragTarget of
        EditorsHandle ->
            Sub.batch
                [ Mouse.moves EditorDragged
                , Mouse.ups (\_ -> EditorDragEnded)
                ]

        LogsHandle ->
            Sub.batch
                [ Mouse.moves LogsDragged
                , Mouse.ups (\_ -> LogsDragEnded)
                ]

        OutputHandle ->
            Sub.batch
                [ Mouse.moves ResultDragged
                , Mouse.ups (\_ -> ResultDragEnded)
                ]

        NoTarget ->
            Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSizeChanged
        , drags model
        , pagesEditorLayoutSubscriptionsIn (decodeMsg model)
        ]
