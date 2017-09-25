port module Pages.Editor.Layout.Subscriptions exposing (subscriptions)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Mouse
import Pages.Editor.Layout.Model as Model exposing (Model)
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
            Debug.crash message


windowMessageDecoder : Model -> Decoder Msg
windowMessageDecoder model =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "mouseup" ->
                        if model.resultDragging then
                            Decode.succeed ResultDragEnded
                        else if model.editorDragging then
                            Decode.succeed EditorDragEnded
                        else
                            Decode.succeed NoOp

                    "mousemove" ->
                        Decode.decode Mouse.Position
                            |> Decode.required "x" Decode.int
                            |> Decode.required "y" Decode.int
                            |> Decode.map
                                (\position ->
                                    if model.resultDragging then
                                        ResultDragged position
                                    else if model.editorDragging then
                                        EditorDragged position
                                    else
                                        NoOp
                                )

                    _ ->
                        Decode.succeed NoOp
            )


editorDrags : Model -> Sub Msg
editorDrags model =
    if model.editorDragging then
        Sub.batch
            [ Mouse.moves EditorDragged
            , Mouse.ups (\_ -> EditorDragEnded)
            ]
    else
        Sub.none


resultDrags : Model -> Sub Msg
resultDrags model =
    if model.resultDragging then
        Sub.batch
            [ Mouse.moves ResultDragged
            , Mouse.ups (\_ -> ResultDragEnded)
            ]
    else
        Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSizeChanged
        , editorDrags model
        , resultDrags model
        , pagesEditorLayoutSubscriptionsIn (decodeMsg model)
        ]
