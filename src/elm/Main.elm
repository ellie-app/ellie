module Main exposing (main)

import String
import Html exposing (Html, div, text, textarea, iframe, button)
import Html.Attributes exposing (value, style, srcdoc, sandbox, disabled, class)
import Html.Events exposing (onClick, onInput)
import Http
import LoadState exposing (LoadState(..))
import Api exposing (Session, Error)
import Effects
import Utils
import CodeMirror


-- MODEL


type Language
    = Elm
    | Css
    | Html


type alias Model =
    { session : LoadState Error Session
    , editors : List ( Language, String, Float, Bool )
    , result : LoadState Error String
    , hasChanged : Bool
    }


canCompile : Model -> Bool
canCompile model =
    model.result == Initial || (model.hasChanged && (not <| LoadState.isLoading model.result))


contentForLanguage : Language -> Model -> String
contentForLanguage language model =
    model.editors
        |> Utils.listFind (\( l, _, _, _ ) -> l == language)
        |> Maybe.map (\( _, c, _, _ ) -> c)
        |> Maybe.withDefault ""



-- UPDATE


type Msg
    = WindowUnloaded
    | TypedInEditor Language String
    | InitComplete (LoadState Error Session)
    | CompileRequested
    | CompileComplete (LoadState Error String)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowUnloaded ->
            case model.session of
                Success session ->
                    ( model
                    , Api.removeSession session
                        |> Http.send (\_ -> NoOp)
                    )

                _ ->
                    ( model, Cmd.none )

        TypedInEditor language content ->
            ( { model
                | editors =
                    model.editors
                        |> List.map
                            (\( l, c, p, i ) ->
                                if l == language then
                                    ( l, content, p, i )
                                else
                                    ( l, c, p, i )
                            )
                , hasChanged =
                    if language == Elm then
                        True
                    else
                        model.hasChanged
              }
            , Cmd.none
            )

        InitComplete sessionResult ->
            ( { model | session = sessionResult }
            , Cmd.none
            )

        CompileRequested ->
            case ( model.session, canCompile model ) of
                ( Success session, True ) ->
                    ( { model | result = Loading }
                    , model.editors
                        |> Utils.listFind (\( l, _, _, _ ) -> l == Elm)
                        |> Maybe.map (\( _, c, _, _ ) -> c)
                        |> Maybe.withDefault ""
                        |> Api.compile session
                        |> Http.send (Api.handleError >> CompileComplete)
                    )

                ( _, _ ) ->
                    ( model, Cmd.none )

        CompileComplete result ->
            ( { model | result = result, hasChanged = not <| LoadState.isSuccess result }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    model.session
        |> LoadState.map (\_ -> Effects.windowUnload WindowUnloaded)
        |> LoadState.withDefault Sub.none



-- INIT


initHtmlContent : String
initHtmlContent =
    """<html>
  <head>
    <title></title>
    <meta charset="utf8" />
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


initCssContent : String
initCssContent =
    """html, body {
  margin: 0;
  background: #F7F7F7;
  font-family: sans-serif;
}

main {
  color: red;
}
"""


initElmContent : String
initElmContent =
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
      , editors =
            [ ( Elm, initElmContent, 1 / 3, False )
            , ( Css, initCssContent, 1 / 3, False )
            , ( Html, initHtmlContent, 1 / 3, False )
            ]
      , result = Initial
      , hasChanged = False
      }
    , Api.createSession
        |> Http.send (Api.handleError >> InitComplete)
    )



-- VIEW


buildSrcDoc : Model -> String -> String
buildSrcDoc model result =
    let
        css =
            contentForLanguage Css model

        html =
            contentForLanguage Html model
    in
        Utils.stringReplace "</head>"
            ("<style>" ++ css ++ "</style><script>" ++ result ++ "</script></head>")
            html


viewResult : Model -> Html Msg
viewResult model =
    case model.result of
        Success result ->
            iframe
                [ sandbox "allow-scripts"
                , srcdoc <| buildSrcDoc model result
                ]
                []

        Initial ->
            div [] [ text "hit the compile button when you're ready!" ]

        Loading ->
            div [] [ text "compiling! just hang on..." ]

        Failure _ ->
            div [] [ text "something went terribly wrong!" ]


viewEditors : List ( Language, String, Float, Bool ) -> Html Msg
viewEditors editorStates =
    let
        viewEditor ( language, content, takeover, collapsed ) =
            CodeMirror.editor
                [ value content
                , style
                    [ ( "height", "300px" )
                    , ( "width", "33.33%" )
                    , ( "display", "inline-block" )
                    ]
                , CodeMirror.onUpdate <| TypedInEditor language
                ]
    in
        editorStates
            |> List.map viewEditor
            |> div [ style [ ( "position", "relative" ) ] ]


viewLoading : Html Msg
viewLoading =
    div [] [ text "plotting out a little patch of server space just for you!" ]


viewLoaded : Model -> Html Msg
viewLoaded model =
    div []
        [ viewEditors model.editors
        , div []
            [ button
                [ disabled <| not <| canCompile model
                , class "mui-btn mui-btn--primary"
                , onClick CompileRequested
                ]
                [ text "Compile" ]
            ]
        , viewResult model
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
