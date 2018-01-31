module Pages.Editor.Header.View
    exposing
        ( Config
        , SaveOption(..)
        , TermsState(..)
        , view
        )

import Colors
import Css exposing (..)
import Css.Foreign
import Data.Ellie.RevisionId as RevisionId exposing (RevisionId)
import Data.Ellie.TermsVersion as TermsVersion exposing (TermsVersion)
import Ellie.Constants as Constants
import Ellie.Ui.Button as Button
import Ellie.Ui.Checkbox as Checkbox
import Ellie.Ui.CopyLink as CopyLink
import Ellie.Ui.Icon as Icon
import Ellie.Ui.Popout as Popout
import Extra.Html as Html
import Html.Styled exposing (Html, Attribute, a, button, div, h1, header, span, text)
import Html.Styled.Attributes as Attributes exposing (css, href, tabindex, title)
import Pages.Editor.Header.Model exposing (Model)
import Pages.Editor.Header.Update exposing (Msg(..))
import Svg.Styled exposing (svg, use)
import Svg.Styled.Attributes as Svg exposing (xlinkHref)


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
    svg [ logoStyles ]
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
                        span [ termsLabelStyles ]
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
                        span [ termsLabelStyles ]
                            [ text "Please accept our "
                            , a [ href <| TermsVersion.link version ] [ text "Terms of Service" ]
                            , text " before saving."
                            ]
                    }
                ]

            Accepted ->
                [ viewLogo
                , div [ buttonStyles ]
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
                , div [ buttonStyles ]
                    [ viewSaveButton config ]
                , div [ buttonStyles ]
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
                , div [ buttonStyles ]
                    [ Popout.view
                        { open = config.model.shareOpen
                        , disabled = config.revisionId == Nothing
                        , onToggle = ToggleShare >> config.mapMsg
                        , tooltip =
                            config.revisionId
                                |> Maybe.map
                                    (\revisionId ->
                                        div []
                                            [ div [ copyLinkContainerStyles ]
                                                [ CopyLink.view
                                                    { id = "direct"
                                                    , url = directLink revisionId
                                                    , title = "Direct Link (Medium, Embed.ly)"
                                                    }
                                                ]
                                            , div [ copyLinkContainerStyles ]
                                                [ CopyLink.view
                                                    { id = "embed"
                                                    , url = embedLink revisionId
                                                    , title = "Embed Link"
                                                    }
                                                ]
                                            , div [ copyLinkContainerStyles ]
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
        [ socialLinkStyles
        , href url
        , Attributes.target "_blank"
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
    header [ headerStyles ]
        [ div [ leftSideStyles ] <| viewLeftSide config
        , div [ rightSideStyles ] <| viewRightSide
        ]


-- STYLES


headerStyles : Attribute msg
headerStyles =
    css
        [ width (pct 100)
        , height (px 40)
        , backgroundColor Colors.darkGray
        , displayFlex
        , alignItems center
        , justifyContent spaceBetween
        , Colors.boxShadow |> .bottom
        , position relative
        , zIndex (int 4)
        , padding2 zero (px 16)
        ]


rightSideStyles : Attribute msg
rightSideStyles =
    css
        [ displayFlex
        , alignItems center
        ]


socialLinkStyles : Attribute msg
socialLinkStyles =
    css
        [ width (px 20)
        , height (px 20)
        , color Colors.lightMediumGray
        , marginRight (px 8)
        , lastChild [ marginRight zero ]
        , hover [ color Colors.lightGray ]
        ]


leftSideStyles : Attribute msg
leftSideStyles =
    css
        [ displayFlex
        , alignItems center
        ]


termsLabelStyles : Attribute msg
termsLabelStyles =
    css
        [ color Colors.lightGray
        , Css.Foreign.descendants
            [ Css.Foreign.a
                [ color Colors.pink
                , textDecoration underline
                ]
            ]
        ]


headerGroupStyles : Attribute msg
headerGroupStyles =
    css
        [ displayFlex
        , alignItems center
        ]


logoStyles : Attribute msg
logoStyles =
    Svg.css
        [ fill Colors.lightGray
        , height (px 20)
        , width (px 51)
        , marginRight (px 24)
        ]


buttonStyles : Attribute msg
buttonStyles =
    css
        [ marginRight (px 16)
        , lastChild [ marginRight zero ]
        , displayFlex
        ]


copyLinkContainerStyles : Attribute msg
copyLinkContainerStyles =
    css
        [ marginBottom (px 8)
        , lastChild [ marginBottom zero ]
        ]
