module Pages.Editor.Types.Example exposing (Example, all, counter, default, helloWorld)


type alias Example =
    { html : String
    , elm : String
    , label : String
    }


default : Example
default =
    helloWorld


helloWorld : Example
helloWorld =
    { label = "Hello World"
    , elm = """module Main exposing (main)

import Html exposing (Html)


main : Html msg
main =
    Html.text "Hello, World!"
"""
    , html = """<html>
<head>
  <style>
    /* you can style your program here */
  </style>
</head>
<body>
  <main></main>
  <script>
    var app = Elm.Main.init({ node: document.querySelector('main') })
    // you can use ports and stuff here
  </script>
</body>
</html>
"""
    }


counter : Example
counter =
    { label = "Counter"
    , html = """<html>
<head>
  <style>
    /* you can style your program here */
  </style>
</head>
<body>
  <main></main>
  <script>
    var app = Elm.Main.init({ node: document.querySelector('main') })
    // you can use ports and stuff here
  </script>
</body>
</html>
"""
    , elm = """module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
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
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
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
