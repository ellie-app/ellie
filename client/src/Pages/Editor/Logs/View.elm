module Pages.Editor.Logs.View exposing (view)

import BoundedDeque
import Colors
import Css exposing (..)
import Data.Ellie.Log as Log exposing (Log)
import Ellie.Ui.Button as Button
import Ellie.Ui.TextInput as TextInput
import Html.Styled exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (id, css)
import Html.Styled.Events exposing (on)
import Json.Decode as Decode
import Pages.Editor.Logs.Model as Model exposing (Model)
import Pages.Editor.Logs.Update exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ containerStyles ]
        [ viewLogs model
        , viewControls model
        ]


onScrolledToBottom : Attribute Msg
onScrolledToBottom =
    on "scroll" <|
        Decode.map3
            (\scrollTop clientHeight scrollHeight ->
                UpdateScrollState <| scrollTop >= scrollHeight - clientHeight
            )
            (Decode.at [ "target", "scrollTop" ] Decode.float)
            (Decode.at [ "target", "clientHeight" ] Decode.float)
            (Decode.at [ "target", "scrollHeight" ] Decode.float)


viewLogs : Model -> Html Msg
viewLogs model =
    div
        [ logsStyles
        , id "pagesEditorLogsLogs"
        , onScrolledToBottom
        ]
        (model.logs
            |> BoundedDeque.filter (.tag >> String.toLower >> String.contains (String.toLower model.search))
            |> BoundedDeque.map viewLog
            |> BoundedDeque.toList
        )


viewLog : Log -> Html Msg
viewLog { tag, body } =
    div [ logStyles ]
        [ div [ labelStyles ] [ text tag ]
        , div [ bodyStyles ] [ text body ]
        ]


viewControls : Model -> Html Msg
viewControls model =
    div [ controlsStyles ]
        [ div [ filterInputStyles ]
            [ TextInput.view
                { placeholder = "Filter by label"
                , value = model.search
                , clearable = True
                , icon = Nothing
                , onChange = SearchChanged
                }
            ]
        , div [ clearButtonStyles ]
            [ Button.view
                { size = Button.Small
                , style = Button.Primary
                , icon = Nothing
                , label = "Clear Logs"
                , disabled = False
                , action = Button.click LogsCleared
                , attributes = []
                }
            ]
        ]


-- STYLES


containerStyles : Attribute msg
containerStyles =
    css
        [ width (pct 100)
        , displayFlex
        , flexDirection column
        ]

controlsStyles : Attribute msg
controlsStyles =
    css
        [ displayFlex
        , width (pct 100)
        , padding (px 12)
        , backgroundColor Colors.darkMediumGray
        , flexShrink (int 0)
        ]


filterInputStyles : Attribute msg
filterInputStyles =
    css
        [ width (pct 100)
        , flexShrink (int 1)
        , paddingRight (px 12)
        ]


clearButtonStyles : Attribute msg
clearButtonStyles =
    css
        [ flexShrink (int 0)
        ]


logsStyles : Attribute msg
logsStyles =
    css
        [ height (pct 100)
        , flexShrink (int 1)
        , displayFlex
        , padding (px 12)
        , overflowY auto
        , flexDirection column
        ]


logStyles : Attribute msg
logStyles =
    css
        [ backgroundColor Colors.darkMediumGray
        , Colors.boxShadow |> .bottom
        , padding (px 12)
        , borderRadius (px 2)
        , marginBottom (px 12)
        , lastChild [ marginBottom zero ]
        ]


labelStyles : Attribute msg
labelStyles =
    css
        [ color Colors.lightMediumGray
        , fontWeight bold
        , fontSize (px 14)
        , marginBottom (px 8)
        ]


bodyStyles : Attribute msg
bodyStyles =
    css
        [ color Colors.lightGray
        , fontSize (px 18)
        , fontFamily monospace
        ]
