module Pages.Embed.View exposing (view)

import Data.Ellie.Revision as Revision exposing (Revision, Snapshot(..))
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Elm.Compiler.Error as CompilerError
import Html exposing (Html, a, button, div, header, iframe, span, text)
import Html.Attributes exposing (href, src, style, target)
import Html.Events exposing (onClick)
import Pages.Embed.Classes exposing (..)
import Pages.Embed.Model as Model exposing (Model, Tab(..))
import Pages.Embed.Routing exposing (Route(..))
import Pages.Embed.Update as Update exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Shared.Constants as Constants
import Shared.Icons as Icons
import Views.Editors.View as Editors
import Views.Output.View as Output


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
            [ ( HeaderTab, True )
            , ( HeaderTabActive, activeTab == myTab )
            ]
        ]
        [ div [ class [ HeaderTabInner ] ]
            [ span [ class [ HeaderTabIcon ] ]
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
                [ class [ HeaderLink, HeaderLinkInner ]
                , href <| Constants.editorBase ++ "/" ++ projectId ++ "/" ++ toString revisionNumber
                , target "_blank"
                ]
                [ span [ class [ HeaderLinkIcon ] ]
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
    Constants.cdnBase ++ "/revisions/" ++ projectId ++ "/" ++ toString revisionNumber ++ ".html"


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
    div
        [ classList
            [ ( LoadedContainer, True )
            , ( LoadingContainer, not (RemoteData.isSuccess model.revision) )
            ]
        ]
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
                    revision.elmCode
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
                        viewLoaded
                            model
                            Revision.empty
        ]
