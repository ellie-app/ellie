module Pages.Editor.Types.Example exposing (Example, all, helloWorld)


type alias Example =
    { html : String
    , elm : String
    , label : String
    }


defaultHtml : String
defaultHtml =
    """<html>
<head>
  <style>
    /* you can style your program here */
  </style>
</head>
<body>
  <script>
    var app = Elm.Main.fullscreen()
    // you can use ports and stuff here
  </script>
</body>
</html>
"""


helloWorld : Example
helloWorld =
    { label = "Hello World"
    , elm = """module Main exposing (main)

import Html exposing (Html, text)


main : Html msg
main =
    text <| Debug.log "hi" "Hello, World!"
"""
    , html = defaultHtml
    }


counter : Example
counter =
    { label = "Counter (Beginner Program)"
    , html = defaultHtml
    , elm = """module Main exposing (main)

import Html exposing (Html, text, button, div)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


model : Model
model =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| toString model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
"""
    }


all : List Example
all =
    [ helloWorld
    , counter
    ]
