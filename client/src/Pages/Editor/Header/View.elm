module Pages.Editor.Header.View
    exposing
        ( Config
        , SaveOption(..)
        , TermsState(..)
        , view
        )

import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Ellie.Constants as Constants
import Ellie.Ui.Button as Button
import Ellie.Ui.Checkbox as Checkbox
import Ellie.Ui.CopyLink as CopyLink
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Popout as Popout
import Extra.Html as Html
import Html exposing (Html, a, button, div, h1, header, span, text)
import Html.Attributes exposing (href, tabindex, target, title)
import Pages.Editor.Header.Model exposing (Model)
import Pages.Editor.Header.Styles as Styles
import Pages.Editor.Header.Update exposing (Msg(..))
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)


type SaveOption
    = Save
    | Update
    | Fork
    | Saving


type TermsState msg
    = Visible (TermsVersion -> msg) TermsVersion
    | Accepting TermsVersion
    | Accepted


type alias Config msg =
    { compileButtonEnabled : Bool
    , embedLinkButtonEnabled : Bool
    , saveButtonEnabled : Bool
    , saveButtonOption : SaveOption
    , buttonsVisible : Bool
    , revisionId : Maybe RevisionId
    , termsState : TermsState msg
    , onSave : msg
    , onCompile : msg
    , onFormat : msg
    , model : Model
    , mapMsg : Msg -> msg
    }


directLink : RevisionId -> String
directLink revisionId =
    Constants.editorBase
        ++ "/"
        ++ revisionId.projectId
        ++ "/"
        ++ toString revisionId.revisionNumber


iframe : RevisionId -> String
iframe revisionId =
    "<iframe src=\""
        ++ embedLink revisionId
        ++ "\" style=\"width:100%; height:400px; border:0; overflow:hidden;\""
        ++ " sandbox=\"allow-modals allow-forms allow-popups allow-scripts allow-same-origin\""
        ++ "></iframe>"


embedLink : RevisionId -> String
embedLink revisionId =
    Constants.embedBase
        ++ "/"
        ++ revisionId.projectId
        ++ "/"
        ++ toString revisionId.revisionNumber


viewLogo : Html msg
viewLogo =
    svg [ Styles.logo ]
        [ use [ xlinkHref "#ellie-logo" ] [] ]


viewSaveButton : Config msg -> Html msg
viewSaveButton config =
    case config.saveButtonOption of
        Fork ->
            Button.view
                { style = Button.Link
                , size = Button.Medium
                , icon = Just Icon.Fork
                , label = "FORK"
                , disabled = not config.saveButtonEnabled
                , attributes = []
                , action = Button.click config.onSave
                }

        Update ->
            Button.view
                { style = Button.Link
                , size = Button.Medium
                , icon = Just Icon.Upload
                , label = "UPDATE"
                , disabled = not config.saveButtonEnabled
                , attributes = []
                , action = Button.click config.onSave
                }

        Save ->
            Button.view
                { style = Button.Link
                , size = Button.Medium
                , icon = Just Icon.Upload
                , label = "SAVE"
                , disabled = not config.saveButtonEnabled
                , attributes = []
                , action = Button.click config.onSave
                }

        Saving ->
            Button.view
                { style = Button.Link
                , size = Button.Medium
                , icon = Just Icon.Upload
                , label = "SAVING..."
                , disabled = True
                , attributes = []
                , action = Button.click config.onSave
                }


viewLeftSide : Config msg -> List (Html msg)
viewLeftSide config =
    if config.buttonsVisible then
        case config.termsState of
            Visible onAccept version ->
                [ viewLogo
                , Checkbox.view
                    { onChange = \_ -> onAccept version
                    , checked = False
                    , id = "accept_terms"
                    , label =
                        span [ Styles.termsLabel ]
                            [ text "Please accept our "
                            , a [ href <| TermsVersion.link version ] [ text "Terms of Service" ]
                            , text " before saving."
                            ]
                    }
                ]

            Accepting version ->
                [ viewLogo
                , Checkbox.view
                    { onChange = \_ -> config.mapMsg NoOp
                    , checked = True
                    , id = "accept_terms"
                    , label =
                        span [ Styles.termsLabel ]
                            [ text "Please accept our "
                            , a [ href <| TermsVersion.link version ] [ text "Terms of Service" ]
                            , text " before saving."
                            ]
                    }
                ]

            Accepted ->
                [ viewLogo
                , div [ Styles.button ]
                    [ Button.view
                        { style = Button.Link
                        , size = Button.Medium
                        , icon = Just Icon.Play
                        , label = "COMPILE"
                        , disabled = not config.compileButtonEnabled
                        , attributes = []
                        , action = Button.click config.onCompile
                        }
                    ]
                , div [ Styles.button ]
                    [ viewSaveButton config ]
                , div [ Styles.button ]
                    [ Button.view
                        { style = Button.Link
                        , size = Button.Medium
                        , icon = Just Icon.Format
                        , label = "FORMAT"
                        , disabled = False
                        , attributes = []
                        , action = Button.click config.onFormat
                        }
                    ]
                , div [ Styles.button ]
                    [ Popout.view
                        { open = config.model.shareOpen
                        , disabled = config.revisionId == Nothing
                        , onToggle = ToggleShare >> config.mapMsg
                        , tooltip =
                            config.revisionId
                                |> Maybe.map
                                    (\revisionId ->
                                        div []
                                            [ div [ Styles.copyLinkContainer ]
                                                [ CopyLink.view
                                                    { id = "direct"
                                                    , url = directLink revisionId
                                                    , title = "Direct Link (Medium, Embed.ly)"
                                                    }
                                                ]
                                            , div [ Styles.copyLinkContainer ]
                                                [ CopyLink.view
                                                    { id = "embed"
                                                    , url = embedLink revisionId
                                                    , title = "Embed Link"
                                                    }
                                                ]
                                            , div [ Styles.copyLinkContainer ]
                                                [ CopyLink.view
                                                    { id = "iframe"
                                                    , url = iframe revisionId
                                                    , title = "IFrame"
                                                    }
                                                ]
                                            ]
                                    )
                                |> Html.maybe
                        , content =
                            Button.view
                                { style = Button.Link
                                , size = Button.Medium
                                , icon = Just Icon.Link
                                , label = "SHARE"
                                , disabled = config.revisionId == Nothing
                                , attributes = []
                                , action = Button.none
                                }
                        }
                    ]
                ]
    else
        [ viewLogo ]


viewSocialLink : String -> String -> Icon.Icon -> Html msg
viewSocialLink url description icon =
    a
        [ Styles.socialLink
        , href url
        , target "_blank"
        , title description
        ]
        [ Icon.view icon ]


viewRightSide : List (Html msg)
viewRightSide =
    [ viewSocialLink "https://github.com/lukewestby/ellie" "Repository" Icon.GitHub
    , viewSocialLink "https://trello.com/b/7cFN60SP/ellie" "Roadmap" Icon.Trello
    ]


view : Config msg -> Html msg
view config =
    header [ Styles.header ]
        [ div [ Styles.leftSide ] <| viewLeftSide config
        , div [ Styles.rightSide ] <| viewRightSide
        ]
