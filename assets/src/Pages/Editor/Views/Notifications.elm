module Pages.Editor.Views.Notifications exposing (..)

import Css exposing (..)
import Css.Foreign
import Data.Url as Url exposing (Url)
import Ellie.Ui.Button as Button
import Ellie.Ui.CopyText as CopyText
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Theme as Theme
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Pages.Editor.Types.EditorAction as EditorAction exposing (EditorAction)
import Pages.Editor.Types.Notification as Notification exposing (Notification)


type alias Config msg =
    { onClose : Notification -> msg
    , onEditorAction : EditorAction -> msg
    , onCloseAll : msg
    , notifications : List Notification
    }


view : Config msg -> Html msg
view config =
    Html.div
        [ css
            [ position absolute
            , right zero
            , bottom zero
            , maxHeight (pct 100)
            , padding (px 16)
            ]
        ]
        (List.map (viewNotification config) config.notifications)


viewNotification : Config msg -> Notification -> Html msg
viewNotification config notification =
    Html.div
        [ css
            [ width (px 400)
            , padding (px 16)
            , backgroundColor Theme.primaryBackground
            , marginBottom (px 16)
            , boxShadow5 (px 0) (px 1) (px 6) (px 0) (hex "#000")
            , lastChild [ marginBottom zero ]
            , hover
                [ Css.Foreign.descendants
                    [ Css.Foreign.selector "[data-close-all-button]" [ displayFlex ]
                    ]
                ]
            ]
        ]
        [ Html.div
            [ css
                [ displayFlex
                , alignItems center
                , paddingBottom (px 16)
                ]
            ]
            [ viewSeverity notification.severity
            , viewTitle notification.title
            , viewCloseAllButton config.onCloseAll
            , viewCloseButton (config.onClose notification)
            ]
        , Html.div
            [ css
                [ color Theme.primaryForeground
                , property "word-wrap" "break-word"
                ]
            ]
            [ Html.text notification.message
            ]
        , if List.isEmpty notification.actions then
            Html.text ""
          else
            Html.div
                [ css [ paddingTop (px 12) ] ]
                (List.map (viewAction config) notification.actions)
        ]


viewAction : Config msg -> Notification.Action -> Html msg
viewAction config action =
    case action of
        Notification.CopyLink url ->
            CopyText.view <| Url.toString url

        Notification.GoToLink url ->
            Html.div []
                [ Html.a
                    [ Attributes.href url
                    , Attributes.target "_blank"
                    , css
                        [ property "word-break" "break-word"
                        , color Theme.accent
                        ]
                    ]
                    [ Html.text url
                    ]
                ]

        Notification.PerformAction label action ->
            Html.div []
                [ Button.view
                    { icon = Nothing
                    , label = label
                    , action = Button.click <| config.onEditorAction action
                    }
                ]


viewSeverity : Notification.Severity -> Html msg
viewSeverity severity =
    Html.div
        [ css
            [ width (px 16)
            , height (px 16)
            , flexShrink zero
            , color <|
                case severity of
                    Notification.Failure ->
                        Theme.failure

                    Notification.Success ->
                        Theme.success

                    Notification.Warning ->
                        Theme.warning

                    Notification.Info ->
                        Theme.information
            ]
        ]
        [ Icon.view <|
            case severity of
                Notification.Failure ->
                    Icon.Failure

                Notification.Success ->
                    Icon.Success

                Notification.Warning ->
                    Icon.Warning

                Notification.Info ->
                    Icon.Info
        ]


viewTitle : String -> Html msg
viewTitle title =
    Html.div
        [ css
            [ width (pct 100)
            , fontSize (px 16)
            , color Theme.primaryForeground
            , fontWeight bold
            , padding2 zero (px 8)
            ]
        ]
        [ Html.text title
        ]


viewCloseAllButton : msg -> Html msg
viewCloseAllButton clickMsg =
    Html.button
        [ css
            [ flexShrink zero
            , property "background" "none"
            , outline zero
            , border zero
            , padding zero
            , color Theme.secondaryForeground
            , hover [ color Theme.primaryForeground ]
            , active [ transform <| scale 1.2 ]
            , width (px 16)
            , height (px 16)
            , cursor pointer
            , display none
            , marginRight (px 8)
            ]
        , Attributes.attribute "data-close-all-button" ""
        , Attributes.title "Close all notifications (Esc)"
        , Events.onClick clickMsg
        ]
        [ Icon.view Icon.CloseAll
        ]


viewCloseButton : msg -> Html msg
viewCloseButton clickMsg =
    Html.button
        [ css
            [ flexShrink zero
            , property "background" "none"
            , outline zero
            , border zero
            , padding zero
            , color Theme.secondaryForeground
            , hover [ color Theme.primaryForeground ]
            , active [ transform <| scale 1.2 ]
            , width (px 16)
            , height (px 16)
            , cursor pointer
            , padding (px 2)
            ]
        , Attributes.title "Close notification"
        , Events.onClick clickMsg
        ]
        [ Icon.view Icon.Close
        ]
