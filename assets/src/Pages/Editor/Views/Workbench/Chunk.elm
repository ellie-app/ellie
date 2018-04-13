module Pages.Editor.Views.Workbench.Chunk exposing (..)

import Css exposing (..)
import Ellie.Ui.Theme as Theme
import Elm.Error as Error
import Html.Styled as Html exposing (Html)


view : Error.Chunk -> Html msg
view chunk =
    case chunk of
        Error.Styled style string ->
            Html.styled Html.span
                [ if style.bold then
                    fontWeight bold
                  else
                    batch []
                , if style.underline then
                    textDecoration underline
                  else
                    batch []
                , style.color
                    |> Maybe.map (chunkColor >> color)
                    |> Maybe.withDefault (color Theme.primaryForeground)
                ]
                []
                [ Html.text string ]

        Error.Unstyled string ->
            Html.span []
                [ Html.text string ]


chunkColor : Error.Color -> Css.Color
chunkColor c =
    case c of
        Error.Red ->
            Theme.red

        Error.RED ->
            Theme.red

        Error.Green ->
            Theme.green

        Error.GREEN ->
            Theme.green

        Error.Blue ->
            Theme.blue

        Error.BLUE ->
            Theme.blue

        Error.Magenta ->
            Theme.pink

        Error.MAGENTA ->
            Theme.pink

        Error.Yellow ->
            Theme.yellow

        Error.YELLOW ->
            Theme.yellow

        Error.Cyan ->
            Theme.blue

        Error.CYAN ->
            Theme.blue

        Error.White ->
            Theme.lightGray

        Error.WHITE ->
            Theme.lightGray

        Error.Black ->
            Theme.darkGray

        Error.BLACK ->
            Theme.darkGray
