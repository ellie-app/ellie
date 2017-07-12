module Pages.Embed.View exposing (view)

import RemoteData exposing (RemoteData(..))
import Html exposing (Html, header, iframe, div, span, button, text, a)
import Html.Attributes exposing (src, style, href, target)
import Html.Events exposing (onClick)
import Pages.Embed.Routing exposing (Route(..))
import Pages.Embed.Update as Update exposing (Msg(..))
import Pages.Embed.Model as Model exposing (Model, Tab(..))
import Pages.Embed.Classes exposing (..)
import Views.Editors.View as Editors
import Views.Output.View as Output
import Data.Ellie.Revision as Revision exposing (Revision, Snapshot(..))
import Data.Elm.Compiler.Error as CompilerError
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Shared.Icons as Icons
import Shared.Constants as Constants


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


viewFailure : String -> Html Msg
viewFailure message =
    div [ class [ FailureContainer ] ]
        [ div [ class [ FailureTitle ] ]
            [ text "Oh no!" ]
        , div [ class [ FailureMessage ] ]
            [ text "Something went wrong while loading this project. The server said:" ]
        , div [ class [ FailureDetails ] ]
            [ text message ]
        ]


viewHeaderButton : Tab -> Tab -> Html Msg -> String -> Html Msg
viewHeaderButton activeTab myTab icon label =
    button
        [ onClick <| SwitchTab myTab
        , classList
            [ ( HeaderButton, True )
            , ( HeaderButtonActive, activeTab == myTab )
            ]
        ]
        [ div [ class [ HeaderButtonInner ] ]
            [ span [ class [ HeaderButtonIcon ] ]
                [ icon ]
            , text label
            ]
        ]


viewHeader : Tab -> RevisionId -> Html Msg
viewHeader activeTab { projectId, revisionNumber } =
    header [ class [ Header ] ]
        [ div [ class [ HeaderLeft ] ]
            [ viewHeaderButton activeTab ElmTab Icons.elmLogo "Elm"
            , viewHeaderButton activeTab HtmlTab Icons.code "HTML"
            , viewHeaderButton activeTab ResultsTab Icons.eye "Results"
            ]
        , div [ class [ HeaderRight ] ]
            [ a
                [ class [ HeaderButton, HeaderButtonInner ]
                , href <| Constants.editorBase ++ "/" ++ projectId ++ "/" ++ toString revisionNumber
                , target "_blank"
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
    Editors.html False Nothing code


viewElm : String -> List CompilerError.Error -> Html Msg
viewElm code errors =
    Editors.elm False Nothing code errors


iframeSrc : RevisionId -> String
iframeSrc { projectId, revisionNumber } =
    Constants.cdnBase ++ "/revisions/" ++ projectId ++ "/" ++ (toString revisionNumber) ++ ".html"


viewResultsUploaded : RevisionId -> Html Msg
viewResultsUploaded revisionId =
    iframe
        [ src <| iframeSrc revisionId
        , class [ Iframe ]
        ]
        []


viewResultsErrors : List CompilerError.Error -> Html Msg
viewResultsErrors errors =
    Output.errors errors


viewResults : Revision -> Html Msg
viewResults revision =
    case revision.snapshot of
        Uploaded ->
            Maybe.map viewResultsUploaded revision.id
                |> Maybe.withDefault (text "")

        Errored errors ->
            viewResultsErrors errors

        _ ->
            text ""


viewLoaded : Model -> Revision -> Html Msg
viewLoaded model revision =
    div [ class [ LoadedContainer ] ]
        [ case model.currentRoute of
            SpecificRevision revisionId ->
                viewHeader model.tab revisionId

            _ ->
                text ""
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

            SpecificRevision _ ->
                case model.revision of
                    Success revision ->
                        viewLoaded
                            model
                            revision

                    Failure error ->
                        viewFailure error.explanation

                    _ ->
                        viewLoading
        ]
