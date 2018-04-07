module Pages.Embed.View exposing (view)

import Colors
import Css exposing (..)
import Css.Foreign
import Data.Ellie.Revision as Revision exposing (Revision, Snapshot(..))
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Elm.Compiler.Error as CompilerError
import Ellie.Constants as Constants
import Ellie.Ui.CodeEditor as CodeEditor
import Ellie.Ui.CompileError as ErrorView
import Ellie.Ui.Icon as Icon
import Extra.Css exposing (blur, filter, withAlpha)
import Extra.Html.Attributes as Attributes
import Html.Styled exposing (Attribute, Html, a, button, div, header, iframe, span, text)
import Html.Styled.Attributes as Attributes exposing (css, href, id, src, style)
import Html.Styled.Events exposing (onClick)
import Pages.Embed.Model as Model exposing (Model, Tab(..))
import Pages.Embed.Routing exposing (Route(..))
import Pages.Embed.Update as Update exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import Svg.Styled exposing (svg, use)
import Svg.Styled.Attributes as Svg exposing (xlinkHref)


viewNotFound : Html Msg
viewNotFound =
    div [ failureContainerStyles ]
        [ div [ failureTitleStyles ]
            [ text "Not found!" ]
        , div [ failureMessageStyles ]
            [ text "We couldn't find the project you asked for." ]
        ]


viewFailure : String -> Html Msg
viewFailure message =
    div [ failureContainerStyles ]
        [ div [ failureTitleStyles ]
            [ text "Oh no!" ]
        , div [ failureMessageStyles ]
            [ text "Something went wrong while loading this project. The server said:" ]
        , div [ failureDetailsStyles ]
            [ text message ]
        ]


viewHeaderButton : Tab -> Tab -> Icon.Icon -> String -> Html Msg
viewHeaderButton activeTab myTab icon label =
    button
        [ onClick <| SwitchTab myTab
        , headerTabStyles
        , Attributes.cond headerTabActiveStyles <| activeTab == myTab
        ]
        [ div [ headerTabInnerStyles ]
            [ span [ headerTabIconStyles ] [ Icon.view icon ]
            , text label
            ]
        ]


viewHeader : Tab -> RevisionId -> Html Msg
viewHeader activeTab { projectId, revisionNumber } =
    header [ headerStyles ]
        [ div [ headerLeftStyles ]
            [ viewHeaderButton activeTab ElmTab Icon.ElmLogo "Elm"
            , viewHeaderButton activeTab HtmlTab Icon.HtmlTag "HTML"
            , viewHeaderButton activeTab ResultsTab Icon.Eye "Results"
            ]
        , div [ headerRightStyles ]
            [ a
                [ headerLinkStyles
                , headerLinkInnerStyles
                , href <| Constants.editorBase ++ "/" ++ projectId ++ "/" ++ toString revisionNumber
                , Attributes.target "_blank"
                ]
                [ span [ headerLinkIconStyles ] [ Icon.view Icon.External ]
                , span [] [ text "Edit on " ]
                , svg [ headerLinkLogoStyles ]
                    [ use [ xlinkHref "#ellie-logo" ] [] ]
                ]
            ]
        ]


viewHtml : String -> Html Msg
viewHtml code =
    CodeEditor.view
        [ CodeEditor.mode "htmlmixed"
        , CodeEditor.readOnly
        , CodeEditor.value code
        , CodeEditor.tabSize 2
        ]


viewElm : String -> List CompilerError.Error -> Html Msg
viewElm code errors =
    CodeEditor.view
        [ CodeEditor.mode "elm"
        , CodeEditor.readOnly
        , CodeEditor.value code
        , CodeEditor.tabSize 4
        ]


iframeSrc : RevisionId -> String
iframeSrc { projectId, revisionNumber } =
    Constants.cdnBase ++ "/revisions/" ++ projectId ++ "/" ++ toString revisionNumber ++ ".html"


viewResultsUploaded : RevisionId -> Html Msg
viewResultsUploaded revisionId =
    iframe
        [ src <| iframeSrc revisionId
        , iframeStyles
        ]
        []


viewResultsErrors : List CompilerError.Error -> Html Msg
viewResultsErrors errors =
    div [ errorsStyles ] <|
        List.map
            (ErrorView.view >> List.singleton >> div [ errorStyles ])
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
        [ loadedContainerStyles
        , Attributes.cond loadingContainerStyles <| not (RemoteData.isSuccess model.revision)
        ]
        [ case model.currentRoute of
            SpecificRevision revisionId ->
                viewHeader model.tab revisionId

            _ ->
                text ""
        , div [ workAreaStyles ]
            [ div
                [ Attributes.cond workAreaTabHiddenStyles <| ElmTab /= model.tab
                , workAreaTabStyles
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
                [ Attributes.cond workAreaTabHiddenStyles <| HtmlTab /= model.tab
                , workAreaTabStyles
                ]
                [ viewHtml revision.htmlCode ]
            , div
                [ Attributes.cond workAreaTabHiddenStyles <| ResultsTab /= model.tab
                , workAreaTabStyles
                ]
                [ viewResults revision ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ containerStyles ]
        [ globalStyles
        , case model.currentRoute of
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



-- STYLES


globalStyles : Html msg
globalStyles =
    Css.Foreign.global
        [ Css.Foreign.html [ height (pct 100) ]
        , Css.Foreign.body
            [ height (pct 100)
            , margin zero
            , fontWeight (int 300)
            , fontFamilies [ Constants.sansFont ]
            ]
        , Css.Foreign.everything
            [ boxSizing borderBox ]
        ]


containerStyles : Attribute msg
containerStyles =
    css
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , zIndex (int 1)
        , overflow hidden
        , borderRadius (px 3)
        ]


failureContainerStyles : Attribute msg
failureContainerStyles =
    css
        [ width (pct 100)
        , height (pct 100)
        , displayFlex
        , flexDirection column
        , alignItems center
        , padding (px 16)
        , paddingTop (px 100)
        , backgroundColor Colors.mediumGray
        ]


failureTitleStyles : Attribute msg
failureTitleStyles =
    css
        [ fontFamilies [ Constants.scriptFont ]
        , color Colors.lightGray
        , fontSize (px 48)
        , lineHeight (px 48)
        , paddingBottom (px 24)
        ]


failureMessageStyles : Attribute msg
failureMessageStyles =
    css
        [ fontSize (px 24)
        , color Colors.lightGray
        , padding2 zero (px 16)
        , maxWidth (px 528)
        , textAlign center
        , paddingBottom (px 24)
        ]


failureDetailsStyles : Attribute msg
failureDetailsStyles =
    css
        [ maxWidth (px 600)
        , backgroundColor Colors.darkGray
        , color Colors.lightGray
        , borderRadius (px 3)
        , padding (px 12)
        , fontSize (px 16)
        ]


headerStyles : Attribute msg
headerStyles =
    css
        [ backgroundColor Colors.darkGray
        , borderBottom3 (px 1) solid Colors.pink
        , displayFlex
        , height (px 40)
        , property "justify-content" "space-between"
        , boxShadow4 zero zero (px 5) (rgba 57 70 78 0.7)
        , property "z-index" "1"
        , position relative
        ]


headerLeftStyles : Attribute msg
headerLeftStyles =
    css
        [ displayFlex ]


headerRightStyles : Attribute msg
headerRightStyles =
    css
        [ padding (px 6) ]


headerTabStyles : Attribute msg
headerTabStyles =
    css
        [ padding2 zero (px 12)
        , paddingTop (px 2)
        , height (pct 100)
        , color Colors.lightGray
        , fontSize (px 16)
        , lineHeight (px 16)
        , border zero
        , cursor pointer
        , margin zero
        , textDecoration none
        , outline zero
        , fontFamily inherit
        , property "-webkit-appearance" "none"
        , lastChild
            [ marginRight zero
            ]
        , backgroundColor transparent
        , disabled
            [ opacity (num 0.5)
            , cursor notAllowed
            ]
        , hover
            [ Colors.mediumGray
                |> withAlpha 0.2
                |> backgroundColor
            ]
        , active
            [ Colors.mediumGray
                |> withAlpha 0.35
                |> backgroundColor
            ]
        ]


headerTabActiveStyles : Attribute msg
headerTabActiveStyles =
    css
        [ borderTop3 (px 2) solid Colors.pink
        , paddingTop zero
        , color Colors.lightGray
        , Colors.mediumGray
            |> withAlpha 0.5
            |> backgroundColor
        ]


headerLinkStyles : Attribute msg
headerLinkStyles =
    css
        [ padding2 (px 6) (px 12)
        , borderRadius (px 3)
        , color Colors.lightGray
        , fontSize (px 16)
        , lineHeight (px 16)
        , textTransform uppercase
        , border zero
        , cursor pointer
        , margin zero
        , textDecoration none
        , outline zero
        , fontFamily inherit
        , property "-webkit-appearance" "none"
        , Colors.mediumGray
            |> withAlpha 0.3
            |> backgroundColor
        , hover
            [ Colors.mediumGray
                |> withAlpha 0.5
                |> backgroundColor
            ]
        , active
            [ Colors.mediumGray
                |> withAlpha 0.7
                |> backgroundColor
            ]
        ]


headerTabInnerStyles : Attribute msg
headerTabInnerStyles =
    css
        [ alignItems center
        , displayFlex
        ]


headerLinkInnerStyles : Attribute msg
headerLinkInnerStyles =
    css
        [ alignItems center
        , displayFlex
        ]


headerTabIconStyles : Attribute msg
headerTabIconStyles =
    css
        [ width (px 16)
        , height (px 16)
        , marginRight (px 8)
        ]


headerLinkIconStyles : Attribute msg
headerLinkIconStyles =
    css
        [ width (px 16)
        , height (px 16)
        , marginRight (px 8)
        ]


headerLinkLogoStyles : Attribute msg
headerLinkLogoStyles =
    Svg.css
        [ marginLeft (px 6)
        , width (px 41)
        , height (px 16)
        , fill Colors.lightGray
        ]


loadedContainerStyles : Attribute msg
loadedContainerStyles =
    css
        [ width (pct 100)
        , height (pct 100)
        , position relative
        , property "transition" "filter 0.3s 0.2s"
        ]


loadingContainerStyles : Attribute msg
loadingContainerStyles =
    css
        [ filter <| blur (px 30)
        ]


workAreaStyles : Attribute msg
workAreaStyles =
    css
        [ property "height" "calc(100% - 40px)"
        , position relative
        ]


workAreaTabStyles : Attribute msg
workAreaTabStyles =
    css
        [ position absolute
        , height (pct 100)
        , top zero
        , left zero
        , width (pct 100)
        , overflow hidden
        ]


workAreaTabHiddenStyles : Attribute msg
workAreaTabHiddenStyles =
    css
        [ property "visibility" "collapse"
        ]


iframeStyles : Attribute msg
iframeStyles =
    css
        [ border zero
        , position relative
        , width (pct 100)
        , backgroundColor (hex "fff")
        , height (pct 100)
        ]


errorsStyles : Attribute msg
errorsStyles =
    css
        [ displayFlex
        , padding (px 16)
        , flexDirection column
        , alignItems center
        , position relative
        ]


errorStyles : Attribute msg
errorStyles =
    css
        [ marginBottom (px 16)
        , lastChild [ marginBottom (px 16) ]
        , maxWidth (px 500)
        , width (pct 100)
        ]
