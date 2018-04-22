module Ellie.Ui.Errors exposing (..)

import Css exposing (..)
import Ellie.Ui.Theme as Theme
import Elm.Error as Error
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events


type alias Config msg =
    { error : Error.Error
    , onPositionClick : Error.Position -> msg
    }


view : Config msg -> Html msg
view config =
    Html.styled Html.div
        [ padding2 zero (px 2)
        , width (pct 100)
        ]
        []
        [ case config.error of
            Error.GeneralProblem { title, message } ->
                Html.styled Html.div
                    [ padding (px 12)
                    , marginBottom (px 2)
                    , backgroundColor Theme.primaryBackground
                    , color Theme.primaryForeground
                    ]
                    []
                    [ Html.styled Html.div
                        [ textTransform capitalize
                        , fontSize (px 14)
                        , fontWeight bold
                        , color Theme.secondaryForeground
                        , marginBottom (px 8)
                        ]
                        []
                        [ Html.text title ]
                    , Html.styled Html.div
                        [ whiteSpace preWrap
                        , fontSize (px 16)
                        , fontFamily monospace
                        ]
                        []
                        (List.map viewChunk message)
                    ]

            Error.ModuleProblems badModules ->
                Html.div []
                    (badModules
                        |> List.concatMap .problems
                        |> List.map (viewProblem config)
                    )
        ]


viewProblem : Config msg -> Error.Problem -> Html msg
viewProblem config problem =
    Html.styled Html.div
        [ padding (px 12)
        , marginBottom (px 2)
        , backgroundColor Theme.primaryBackground
        , color Theme.primaryForeground
        ]
        []
        [ Html.styled Html.div
            [ textTransform capitalize
            , fontSize (px 14)
            , fontWeight bold
            , color Theme.secondaryForeground
            , marginBottom (px 8)
            ]
            []
            [ Html.text <| String.toLower problem.title ]
        , Html.styled Html.a
            [ color Theme.accent
            , fontSize (px 14)
            , marginBottom (px 12)
            , display inlineBlock
            ]
            [ Attributes.href "javascript:void(0)"
            , Events.onClick <| config.onPositionClick problem.region.start
            ]
            [ Html.text <| "Line " ++ toString problem.region.start.line ++ ", Column " ++ toString problem.region.start.column ]
        , Html.styled Html.div
            [ whiteSpace preWrap
            , fontSize (px 16)
            , fontFamily monospace
            ]
            []
            (List.map viewChunk problem.message)
        ]


viewChunk : Error.Chunk -> Html msg
viewChunk chunk =
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
