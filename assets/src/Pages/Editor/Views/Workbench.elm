module Pages.Editor.Views.Workbench exposing (..)

import BoundedDeque exposing (BoundedDeque)
import Css exposing (..)
import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Constants as Constants
import Ellie.Ui.Button as Button
import Ellie.Ui.Errors as Errors
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Output as Output
import Ellie.Ui.TextInput as TextInput
import Ellie.Ui.Theme as Theme
import Elm.Compiler as Compiler
import Elm.Error as ElmError
import Elm.Version as Version exposing (Version)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Pages.Editor.State.Working exposing (ErrorsPane(..), SuccessPane(..), Workbench(..))
import Pages.Editor.Types.Log as Log exposing (Log)
import Pages.Editor.Types.Revision as Revision exposing (Revision)
import Pages.Editor.Views.Workbench.Share as Share


type alias Config msg =
    { onCompile : Maybe msg
    , onSave : Maybe msg
    , onExpand : msg
    , onIframeReload : msg
    , onClearLogs : msg
    , onDownloadZip : msg
    , onGoToLocation : ElmError.Position -> msg
    , onLogSearchChanged : String -> msg
    , onLogReceived : Log -> msg
    , onSelectErrorsPane : ErrorsPane -> msg
    , onSelectSuccessPane : SuccessPane -> msg
    , onRuntimeException : String -> msg
    , onCanDebugChange : Bool -> msg
    , compilerVersion : Version
    , compiling : Bool
    , workbench : Workbench
    , maximized : Bool
    , token : Jwt
    , saving : Bool
    , revisionId : Maybe Revision.Id
    , hasUnsavedChanges : Bool
    }


view : Config msg -> Html msg
view config =
    Html.div
        [ css
            [ backgroundColor Theme.secondaryBackground
            , width (pct 100)
            , height (pct 100)
            , position relative
            , zIndex (int 0)
            ]
        ]
        [ Html.div
            [ css
                [ zIndex (int 1)
                , position relative
                , width (pct 100)
                , height (pct 100)
                ]
            ]
            [ viewContent config ]
        ]


viewContent : Config msg -> Html msg
viewContent config =
    case config.workbench of
        Ready ->
            viewInitial config

        Finished { pane, logs, logSearch, canDebug, renderedHtml } ->
            Html.div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , height (pct 100)
                    , width (pct 100)
                    , position relative
                    ]
                ]
                [ viewFinishedHeader pane canDebug config
                , Html.div
                    [ css
                        [ height (pct 100)
                        , width (pct 100)
                        , position relative
                        , displayFlex
                        ]
                    ]
                    [ viewOutput (pane == SuccessDebug && canDebug) renderedHtml config
                    , case ( config.revisionId, pane ) of
                        ( _, SuccessLogs ) ->
                            viewLogs logSearch logs config

                        ( Just rid, SuccessShare ) ->
                            Share.view
                                { onDownloadZip = config.onDownloadZip
                                , revisionId = rid
                                , hasUnsavedChanges = config.hasUnsavedChanges
                                }

                        _ ->
                            Html.text ""
                    ]
                ]

        FinishedWithError { error, pane } ->
            Html.div
                [ css
                    [ displayFlex
                    , flexDirection column
                    , height (pct 100)
                    , width (pct 100)
                    , position relative
                    ]
                ]
                [ viewErrorsHeader pane config
                , Html.div
                    [ css
                        [ height (pct 100)
                        , width (pct 100)
                        , position relative
                        , displayFlex
                        ]
                    ]
                    [ case ( config.revisionId, pane ) of
                        ( _, ErrorsList ) ->
                            Errors.view { error = error, onPositionClick = config.onGoToLocation }

                        ( Just rid, ErrorsShare ) ->
                            Share.view
                                { onDownloadZip = config.onDownloadZip
                                , revisionId = rid
                                , hasUnsavedChanges = config.hasUnsavedChanges
                                }

                        _ ->
                            Html.text ""
                    ]
                ]


viewErrorsHeader : ErrorsPane -> Config msg -> Html msg
viewErrorsHeader pane config =
    let
        actions =
            List.filterMap identity
                [ Just
                    { icon =
                        if config.compiling then
                            Just Icon.Loading
                        else
                            Just Icon.Play
                    , label =
                        if config.compiling then
                            "Compiling..."
                        else
                            "Compile"
                    , action =
                        case ( config.compiling, config.onCompile ) of
                            ( False, Just onCompile ) ->
                                Button.click onCompile

                            _ ->
                                Button.none
                    }
                , if Version.eq config.compilerVersion Compiler.version then
                    Just
                        { label =
                            if config.saving then
                                "Saving..."
                            else
                                "Save"
                        , icon =
                            if config.saving then
                                Just Icon.Loading
                            else
                                Just Icon.Upload
                        , action =
                            case config.onSave of
                                Just onSave ->
                                    Button.click onSave

                                Nothing ->
                                    Button.none
                        }
                  else
                    Nothing
                ]

        tabs =
            List.filterMap identity
                [ Just ( config.onSelectErrorsPane ErrorsList, "Errors", pane == ErrorsList )
                , Maybe.map (\_ -> ( config.onSelectErrorsPane ErrorsShare, "Share", pane == ErrorsShare )) config.revisionId
                ]
    in
    viewHeader config actions tabs


viewFinishedHeader : SuccessPane -> Bool -> Config msg -> Html msg
viewFinishedHeader pane canDebug config =
    let
        actions =
            List.filterMap identity
                [ Just
                    { icon =
                        if config.compiling then
                            Just Icon.Loading
                        else
                            Just Icon.Play
                    , label =
                        if config.compiling then
                            "Compiling..."
                        else
                            "Compile"
                    , action =
                        case ( config.compiling, config.onCompile ) of
                            ( False, Just onCompile ) ->
                                Button.click onCompile

                            _ ->
                                Button.none
                    }
                , if Version.eq config.compilerVersion Compiler.version then
                    Just
                        { icon = Just Icon.Upload
                        , label = "Save"
                        , action =
                            case ( config.saving, config.onSave ) of
                                ( False, Just onSave ) ->
                                    Button.click onSave

                                _ ->
                                    Button.none
                        }
                  else
                    Nothing
                , case pane of
                    SuccessOutput ->
                        Just
                            { icon = Just Icon.Reload
                            , label = "Reload"
                            , action = Button.click config.onIframeReload
                            }

                    SuccessDebug ->
                        Nothing

                    SuccessLogs ->
                        Just
                            { icon = Just Icon.Trash
                            , label = "Clear"
                            , action = Button.click config.onClearLogs
                            }

                    SuccessShare ->
                        Nothing
                ]

        tabs =
            List.filterMap identity
                [ Just ( config.onSelectSuccessPane SuccessOutput, "Output", pane == SuccessOutput )
                , if canDebug then
                    Just ( config.onSelectSuccessPane SuccessDebug, "Debug", pane == SuccessDebug )
                  else
                    Nothing
                , Just ( config.onSelectSuccessPane SuccessLogs, "Logs", pane == SuccessLogs )
                , Maybe.map (\_ -> ( config.onSelectSuccessPane SuccessShare, "Share", pane == SuccessShare )) config.revisionId
                ]
    in
    viewHeader config actions tabs


viewHeader : Config msg -> List (Button.Config msg) -> List ( msg, String, Bool ) -> Html msg
viewHeader config actions tabs =
    Html.div
        [ css
            [ width (pct 100)
            , height (px 42)
            , displayFlex
            , backgroundColor Theme.primaryBackground
            , flexShrink zero
            , alignItems center
            , justifyContent flexEnd
            , padding2 zero (px 8)
            , overflow hidden
            , borderBottom3 (px 2) solid Theme.staticBorder
            ]
        ]
        [ Html.button
            [ Attributes.title <|
                if config.maximized then
                    "Minimize workbench"
                else
                    "Maximize workbench"
            , Events.onClick config.onExpand
            , css
                [ border zero
                , property "background" "none"
                , outline zero
                , height (pct 100)
                , padding2 zero (px 8)
                , width (px 28)
                , flexShrink (int 0)
                , color Theme.secondaryForeground
                , hover [ color Theme.primaryForeground ]
                , cursor pointer
                ]
            ]
            [ Html.div
                [ css
                    [ height (pct 100)
                    , if config.maximized then
                        transform <| rotate (deg -90)
                      else
                        transform <| rotate (deg 90)
                    ]
                ]
                [ Icon.view Icon.Chevron ]
            ]
        , viewHeaderActions actions
        , viewHeaderTabs tabs
        ]


viewHeaderActions : List (Button.Config msg) -> Html msg
viewHeaderActions actionDefs =
    Html.div
        [ css
            [ displayFlex
            , flexShrink (int 0)
            , overflowX auto
            , overflowY hidden
            , height (pct 100)
            , alignItems center
            , justifyContent flexStart
            ]
        ]
        (List.map viewHeaderAction actionDefs)


viewHeaderAction : Button.Config msg -> Html msg
viewHeaderAction config =
    Html.div
        [ css
            [ padding2 zero (px 8)
            , height (pct 100)
            , displayFlex
            , alignItems center
            ]
        ]
        [ Button.view config ]


viewHeaderTabs : List ( msg, String, Bool ) -> Html msg
viewHeaderTabs tabDefs =
    Html.div
        [ css
            [ displayFlex
            , flexShrink (int 1)
            , overflowX auto
            , width (pct 100)
            , height (pct 100)
            , alignItems center
            , justifyContent flexEnd
            ]
        ]
        (List.map (\( msg, name, active ) -> viewHeaderTab msg name active) tabDefs)


viewHeaderTab : msg -> String -> Bool -> Html msg
viewHeaderTab msg name active =
    Html.button
        [ css
            [ outline zero
            , border zero
            , property "background" "none"
            , padding2 zero (px 8)
            , displayFlex
            , alignItems center
            , height (pct 100)
            , cursor pointer
            ]
        , Events.onClick msg
        ]
        [ Html.div
            [ css
                [ fontSize (px 14)
                , color Theme.tabForeground
                , padding2 zero (px 2)
                , paddingBottom (px 3)
                , fontWeight bold
                , textTransform uppercase
                , if active then
                    borderBottom3 (px 1) solid Theme.tabActiveBorder
                  else
                    batch []
                ]
            ]
            [ Html.text name ]
        ]


viewInitial : Config msg -> Html msg
viewInitial config =
    Html.div
        [ css
            [ displayFlex
            , flexDirection column
            , alignItems center
            , justifyContent center
            , width (pct 100)
            , height (pct 100)
            ]
        ]
        [ Html.div
            [ css
                [ color Theme.primaryForeground
                , fontSize (px 24)
                ]
            ]
            [ Html.text "Press COMPILE to run your program" ]
        , Html.div
            [ css [ paddingTop (px 24) ]
            ]
            [ Button.view
                { icon =
                    if config.compiling then
                        Just Icon.Loading
                    else
                        Just Icon.Play
                , label =
                    if config.compiling then
                        "Compiling..."
                    else
                        "Compile"
                , action =
                    if config.compiling then
                        Button.none
                    else
                        case config.onCompile of
                            Just onCompile ->
                                Button.click onCompile

                            Nothing ->
                                Button.none
                }
            ]
        ]


viewOutput : Bool -> String -> Config msg -> Html msg
viewOutput debug htmlCode config =
    Output.view
        [ Output.elmSource <| Constants.serverOrigin ++ "/r/workspace?token=" ++ Jwt.toString config.token ++ "&elmVersion=" ++ Version.toString config.compilerVersion
        , Output.onLog config.onLogReceived
        , Output.debug debug
        , Output.html htmlCode
        , Output.onCanDebug config.onCanDebugChange
        , Output.onRuntimeException config.onRuntimeException
        ]


viewLogs : String -> BoundedDeque Log -> Config msg -> Html msg
viewLogs logSearch logs config =
    let
        lowerSearch =
            String.toLower logSearch

        filteredLogs =
            if String.isEmpty lowerSearch then
                BoundedDeque.toList logs
            else
                logs
                    |> BoundedDeque.toList
                    |> List.filter (\l -> String.startsWith lowerSearch (String.toLower l.label))
    in
    Html.div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , backgroundColor Theme.secondaryBackground
            , position absolute
            , top zero
            , left zero
            , displayFlex
            , flexDirection column
            ]
        ]
        [ Html.div
            [ css
                [ padding (px 16)
                , flexShrink (int 0)
                ]
            ]
            [ TextInput.view
                { placeholder = "Filter by label"
                , value = logSearch
                , clearable = True
                , onChange = config.onLogSearchChanged
                , autofocus = False
                , icon = Just Icon.Search
                }
            ]
        , Html.div
            [ css
                [ padding2 zero (px 2)
                , overflowY auto
                ]
            ]
            (List.map viewLog filteredLogs)
        ]


viewLog : Log -> Html msg
viewLog log =
    Html.div
        [ css
            [ width (pct 100)
            , margin2 (px 2) zero
            , backgroundColor Theme.primaryBackground
            , padding (px 12)
            , firstChild [ marginTop zero ]
            , lastChild [ marginBottom zero ]
            ]
        ]
        [ Html.div
            [ css
                [ color Theme.secondaryForeground
                , fontSize (px 14)
                , fontWeight bold
                , paddingBottom (px 12)
                , lineHeight (num 1)
                ]
            ]
            [ Html.text log.label ]
        , Html.div
            [ css
                [ color Theme.primaryForeground
                , fontSize (px 18)
                , lineHeight (num 1)
                , fontFamily Theme.editorFontFamily
                , whiteSpace preWrap
                ]
            ]
            [ Html.text log.body ]
        ]
