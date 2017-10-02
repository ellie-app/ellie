module Pages.Embed.View exposing (view)

import Data.Ellie.Revision as Revision exposing (Revision, Snapshot(..))
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Elm.Compiler.Error as CompilerError
import Ellie.Ui.CompileError as ErrorView
import Ellie.Ui.Icon as Icon
import Extra.Html.Attributes as Attributes
import Html exposing (Html, a, button, div, header, iframe, span, text)
import Html.Attributes exposing (href, id, src, style, target)
import Html.Events exposing (onClick)
import Pages.Embed.Model as Model exposing (Model, Tab(..))
import Pages.Embed.Routing exposing (Route(..))
import Pages.Embed.Update as Update exposing (Msg(..))
import Pages.Embed.View.Styles as Styles
import RemoteData exposing (RemoteData(..))
import Shared.Constants as Constants
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)


viewNotFound : Html Msg
viewNotFound =
    div [ Styles.failureContainer ]
        [ div [ Styles.failureTitle ]
            [ text "Not found!" ]
        , div [ Styles.failureMessage ]
            [ text "We couldn't find the project you asked for." ]
        ]


viewFailure : String -> Html Msg
viewFailure message =
    div [ Styles.failureContainer ]
        [ div [ Styles.failureTitle ]
            [ text "Oh no!" ]
        , div [ Styles.failureMessage ]
            [ text "Something went wrong while loading this project. The server said:" ]
        , div [ Styles.failureDetails ]
            [ text message ]
        ]


viewHeaderButton : Tab -> Tab -> Icon.Icon -> String -> Html Msg
viewHeaderButton activeTab myTab icon label =
    button
        [ onClick <| SwitchTab myTab
        , Styles.headerTab
        , Attributes.cond Styles.headerTabActive <| activeTab == myTab
        ]
        [ div [ Styles.headerTabInner ]
            [ span [ Styles.headerTabIcon ] [ Icon.view icon ]
            , text label
            ]
        ]


viewHeader : Tab -> RevisionId -> Html Msg
viewHeader activeTab { projectId, revisionNumber } =
    header [ Styles.header ]
        [ div [ Styles.headerLeft ]
            [ viewHeaderButton activeTab ElmTab Icon.ElmLogo "Elm"
            , viewHeaderButton activeTab HtmlTab Icon.HtmlTag "HTML"
            , viewHeaderButton activeTab ResultsTab Icon.Eye "Results"
            ]
        , div [ Styles.headerRight ]
            [ a
                [ Styles.headerLink
                , Styles.headerLinkInner
                , href <| Constants.editorBase ++ "/" ++ projectId ++ "/" ++ toString revisionNumber
                , target "_blank"
                ]
                [ span [ Styles.headerLinkIcon ] [ Icon.view Icon.External ]
                , span [] [ text "Edit on " ]
                , svg [ Styles.headerLinkLogo ]
                    [ use [ xlinkHref "#ellie-logo" ] [] ]
                ]
            ]
        ]


viewHtml : String -> Html Msg
viewHtml code =
    div [ id "htmlEditor" ] []


viewElm : String -> List CompilerError.Error -> Html Msg
viewElm code errors =
    div [ id "elmEditor" ] []


iframeSrc : RevisionId -> String
iframeSrc { projectId, revisionNumber } =
    Constants.cdnBase ++ "/revisions/" ++ projectId ++ "/" ++ toString revisionNumber ++ ".html"


viewResultsUploaded : RevisionId -> Html Msg
viewResultsUploaded revisionId =
    iframe
        [ src <| iframeSrc revisionId
        , Styles.iframe
        ]
        []


viewResultsErrors : List CompilerError.Error -> Html Msg
viewResultsErrors errors =
    div [ Styles.errors ] <|
        List.map
            (ErrorView.view >> List.singleton >> div [ Styles.error ])
            errors


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
        [ Styles.loadedContainer
        , Attributes.cond Styles.loadingContainer <| not (RemoteData.isSuccess model.revision)
        ]
        [ case model.currentRoute of
            SpecificRevision revisionId ->
                viewHeader model.tab revisionId

            _ ->
                text ""
        , div [ Styles.workArea ]
            [ div
                [ Attributes.cond Styles.workAreaTabHidden <| ElmTab /= model.tab
                , Styles.workAreaTab
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
                [ Attributes.cond Styles.workAreaTabHidden <| HtmlTab /= model.tab
                , Styles.workAreaTab
                ]
                [ viewHtml revision.htmlCode ]
            , div
                [ Attributes.cond Styles.workAreaTabHidden <| ResultsTab /= model.tab
                , Styles.workAreaTab
                ]
                [ viewResults revision ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ Styles.container ]
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
