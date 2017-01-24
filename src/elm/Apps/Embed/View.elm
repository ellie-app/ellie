module Apps.Embed.View exposing (view)

import RemoteData exposing (RemoteData(..))
import Html exposing (Html, header, iframe, div, span, button, text, a)
import Html.Attributes exposing (src, style, href, target)
import Html.Events exposing (onClick)
import Apps.Embed.Routing exposing (Route(..))
import Apps.Embed.Update as Update exposing (Msg(..))
import Apps.Embed.Model as Model exposing (Model, Tab(..))
import Apps.Embed.Classes exposing (..)
import Components.Editors.View as Editors
import Components.Output.View as Output
import Types.Revision as Revision exposing (Revision, Snapshot(..))
import Types.CompileError as CompileError exposing (CompileError)
import Shared.Icons as Icons


viewLoading : Html Msg
viewLoading =
    div [ class [ LoadingContainer ] ]
        [ div [ class [ LoadingTitle ] ]
            [ text "Loading..." ]
        , div [ class [ LoadingMessage ] ]
            [ text "This should only take a second." ]
        ]


viewNotFound : Html Msg
viewNotFound =
    div [ class [ FailureContainer ] ]
        [ div [ class [ FailureTitle ] ]
            [ text "Not found!" ]
        , div [ class [ FailureMessage ] ]
            [ text "We couldn't find the project you asked for." ]
        ]


viewFailure : Html Msg
viewFailure =
    div [ class [ FailureContainer ] ]
        [ div [ class [ FailureTitle ] ]
            [ text "Oh no!" ]
        , div [ class [ FailureMessage ] ]
            [ text "Something went wrong while loading this project. The server said:" ]
        , div [ class [ FailureDetails ] ]
            [ text "Failed to do whatever or something. This error is generated on the server. It’s meant to provide more details to the user because everyone hates vague errors." ]
        ]


viewHeader : Html Msg
viewHeader =
    header [ class [ Header ] ]
        [ div [ class [ HeaderLeft ] ]
            [ button
                [ onClick <| SwitchTab ElmTab
                , class [ HeaderButton ]
                ]
                [ span [ class [ HeaderButtonIcon ] ]
                    [ Icons.elmLogo ]
                , text "Elm"
                ]
            , button
                [ onClick <| SwitchTab HtmlTab
                , class [ HeaderButton ]
                ]
                [ span [ class [ HeaderButtonIcon ] ]
                    [ Icons.code ]
                , text "HTML"
                ]
            , button
                [ onClick <| SwitchTab ResultsTab
                , class [ HeaderButton ]
                ]
                [ span [ class [ HeaderButtonIcon ] ]
                    [ Icons.eye ]
                , text "Results"
                ]
            ]
        , div [ class [ HeaderRight ] ]
            [ a
                [ class [ HeaderButton ]
                ]
                [ span [ class [ HeaderButtonIcon ] ]
                    [ Icons.edit ]
                , span [] [ text "Edit on " ]
                , span [ class [ HeaderLinkLogo ] ] [ text " Ellie" ]
                ]
            ]
        ]


viewHtml : String -> Html Msg
viewHtml code =
    Editors.html Nothing code


viewElm : String -> List CompileError -> Html Msg
viewElm code errors =
    Editors.elm Nothing code errors


viewResultsUploaded : String -> Int -> Html Msg
viewResultsUploaded projectId revisionNumber =
    iframe
        [ src <| ("https://s3.us-east-2.amazonaws.com/ellie-compile-results/" ++ projectId ++ "-" ++ toString revisionNumber ++ ".html")
        , class []
        ]
        []


viewResultsErrors : List CompileError -> Html Msg
viewResultsErrors errors =
    Output.errors errors


viewResults : Revision -> Html Msg
viewResults revision =
    case revision.snapshot of
        Uploaded ->
            Maybe.map2 viewResultsUploaded
                revision.projectId
                revision.revisionNumber
                |> Maybe.withDefault (text "")

        Errored errors ->
            viewResultsErrors errors

        _ ->
            text ""


activeStyle : Tab -> Tab -> Html.Attribute Msg
activeStyle myTab currentTab =
    if myTab == currentTab then
        style [ ( "height", "100%" ), ( "position", "absolute" ) ]
    else
        style [ ( "visibility", "collapse" ), ( "height", "100%" ), ( "position", "absolute" ) ]


viewLoaded : Model -> Revision -> Html Msg
viewLoaded model revision =
    div [ class [ LoadedContainer ] ]
        [ viewHeader
        , div [ class [ WorkArea ] ]
            [ div
                [ classList
                    [ ( WorkAreaTabHidden, ElmTab /= model.tab )
                    , ( WorkAreaTab, True )
                    ]
                ]
                [ viewElm
                    (revision.elmCode)
                    (case revision.snapshot of
                        Errored errors ->
                            errors

                        _ ->
                            []
                    )
                ]
            , div
                [ classList
                    [ ( WorkAreaTabHidden, HtmlTab /= model.tab )
                    , ( WorkAreaTab, True )
                    ]
                ]
                [ viewHtml revision.htmlCode ]
            , div
                [ classList
                    [ ( WorkAreaTabHidden, ResultsTab /= model.tab )
                    , ( WorkAreaTab, True )
                    ]
                ]
                [ viewResults revision ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class [ Container ] ]
        [ case model.currentRoute of
            NotFound ->
                viewNotFound

            SpecificRevision _ _ ->
                case model.revision of
                    Success revision ->
                        viewLoaded model revision

                    Failure error ->
                        viewFailure

                    _ ->
                        viewLoading
        ]
