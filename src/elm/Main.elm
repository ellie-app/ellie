module Main exposing (main)

import Html exposing (Html, div, text, textarea, iframe, button)
import Html.Attributes exposing (value, style, srcdoc, sandbox, disabled, class)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..))
import RemoteData.Infix exposing ((<*>))
import Api exposing (Session, Error, CompileError)
import Utils
import Effects
import CodeMirror


-- MODEL


type alias Model =
    { session : RemoteData Error Session
    , compileResult : RemoteData Error (List CompileError)
    , htmlCode : String
    , elmCode : String
    }



-- UPDATE


type Msg
    = WindowUnloaded
    | TypedElmCode String
    | TypedHtmlCode String
    | InitCompleted (RemoteData Error Session)
    | Compile
    | CompileCompleted (RemoteData Error (List CompileError))
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowUnloaded ->
            case model.session of
                Success session ->
                    ( model
                    , Api.removeSession session
                        |> Api.send (\_ -> NoOp)
                    )

                _ ->
                    ( model, Cmd.none )

        InitCompleted sessionResult ->
            ( { model | session = sessionResult }
            , Cmd.none
            )

        TypedElmCode nextCode ->
            ( { model | elmCode = nextCode }
            , Cmd.none
            )

        TypedHtmlCode nextCode ->
            ( { model | htmlCode = nextCode }
            , Cmd.none
            )

        Compile ->
            case model.session of
                Success session ->
                    ( { model | compileResult = Loading }
                    , Api.compile session model.elmCode
                        |> Api.send CompileCompleted
                    )

                _ ->
                    ( model, Cmd.none )

        CompileCompleted result ->
            ( { model | compileResult = result }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    model.session
        |> RemoteData.map (\_ -> Effects.windowUnload WindowUnloaded)
        |> RemoteData.withDefault Sub.none



-- INIT


initHtmlCode : String
initHtmlCode =
    """<html>
  <head>
    <title></title>
    <meta charset="utf8" />
    <style>
        html, body {
          margin: 0;
          background: #F7F7F7;
          font-family: sans-serif;
        }

        main {
          color: red;
        }
    </style>
  </head>
  <body>
    <main></main>
    <script>
      var main = document.querySelector('main')
      var app = Elm.PortDemo.embed(main)
      setInterval(function () {
        app.ports.counter.send(1)
      }, 1000)
    </script>
  </body>
</html>
"""


initElmCode : String
initElmCode =
    """port module PortDemo exposing (main)

import Html exposing (text)


port counter : (Int -> msg) -> Sub msg


main =
    Html.program
        { view = \\count -> text <| toString count
        , update = \\next current -> (current + next, Cmd.none)
        , subscriptions = \\_ -> counter identity
        , init = (0, Cmd.none)
        }
"""


init : ( Model, Cmd Msg )
init =
    ( { session = Loading
      , elmCode = initElmCode
      , htmlCode = initHtmlCode
      , compileResult = NotAsked
      }
    , Api.createSession
        |> Api.send InitCompleted
    )



-- VIEW


viewElmEditor : String -> RemoteData Error (List CompileError) -> Html Msg
viewElmEditor content compileResult =
    let
        compileErrorLevelToSeverity level =
            case level of
                "warning" ->
                    CodeMirror.Warning

                _ ->
                    CodeMirror.Error

        compileErrorToLinterMessage compileError =
            CodeMirror.linterMessage
                (compileErrorLevelToSeverity compileError.level)
                (compileError.overview ++ "\n\n" ++ compileError.details)
                (CodeMirror.position (compileError.region.start.line - 1) (compileError.region.start.column - 1))
                (CodeMirror.position (compileError.region.end.line - 1) (compileError.region.end.column))

        linterMessages =
            compileResult
                |> RemoteData.withDefault []
                |> List.map compileErrorToLinterMessage
    in
        CodeMirror.editor
            [ value content
            , CodeMirror.linterMessages linterMessages
            , CodeMirror.onUpdate TypedElmCode
            , style
                [ ( "height", "300px" )
                , ( "width", "50%" )
                , ( "display", "inline-block" )
                ]
            ]


viewHtmlEditor : String -> Html Msg
viewHtmlEditor content =
    CodeMirror.editor
        [ value content
        , CodeMirror.onUpdate TypedHtmlCode
        , style
            [ ( "height", "300px" )
            , ( "width", "50%" )
            , ( "display", "inline-block" )
            ]
        ]


viewEditors : Model -> Html Msg
viewEditors model =
    div [ style [ ( "position", "relative" ) ] ]
        [ viewElmEditor model.elmCode model.compileResult
        , viewHtmlEditor model.htmlCode
        ]


viewResult : String -> RemoteData Error Session -> RemoteData Error (List CompileError) -> Html Msg
viewResult htmlCode sessionData compilerData =
    case (RemoteData.succeed (,)) <*> sessionData <*> compilerData of
        NotAsked ->
            div [] [ text "You ain't seen nothin' yet" ]

        Loading ->
            div [] [ text "Compiling" ]

        Failure error ->
            div [] [ text "Something bad happened!" ]

        Success ( session, result ) ->
            let
                document =
                    Utils.stringReplace
                        ("</head>")
                        ("<script src=\"http://localhost:1337/sessions/" ++ session.id ++ "/result\"></script></head>")
                        (htmlCode)
            in
                div []
                    [ iframe
                        [ srcdoc document
                        , style
                            [ ( "height", "calc(100% - 300px)" )
                            , ( "width", "100%" )
                            , ( "border", "0" )
                            ]
                        ]
                        []
                    ]


viewLoading : Html Msg
viewLoading =
    div [] [ text "plotting out a little patch of server space just for you!" ]


viewLoaded : Model -> Html Msg
viewLoaded model =
    div []
        [ viewEditors model
        , div [] [ button [ onClick Compile ] [ text "Compile" ] ]
        , viewResult model.htmlCode model.session model.compileResult
        ]


viewError : Html Msg
viewError =
    div [] [ text "it looks like we're having some problems. sorry about that!" ]


view : Model -> Html Msg
view model =
    case model.session of
        Success _ ->
            viewLoaded model

        Loading ->
            viewLoading

        _ ->
            viewError



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = subscriptions
        }
