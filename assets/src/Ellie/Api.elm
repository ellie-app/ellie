module Ellie.Api
    exposing
        ( revision
        )

import Data.Jwt as Jwt exposing (Jwt)
import Ellie.Api.Object.Package as ApiPackage
import Ellie.Api.Object.Revision as ApiRevision
import Ellie.Api.Query as ApiQuery
import Ellie.Api.Scalar exposing (Name(..), Uuid(..), Version(..))
import Ellie.Types.Revision as Revision exposing (Revision)
import Ellie.Types.TermsVersion as TermsVersion exposing (TermsVersion)
import Elm.Name as Name
import Elm.Version as Version
import Graphqelm.Field as Field
import Graphqelm.Http as Graphqelm
import Graphqelm.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphqelm.SelectionSet exposing (hardcoded, with)


type alias Error =
    Graphqelm.Error ()


versionField : Field.Field Version a -> Field.Field Version.Version a
versionField =
    Field.mapOrFail (\(Version v) -> Version.fromString v)


uuidField : Field.Field Uuid a -> Field.Field String a
uuidField =
    Field.map (\(Uuid u) -> u)


nameField : Field.Field Name a -> Field.Field Name.Name a
nameField =
    Field.mapOrFail (\(Name n) -> Name.fromString n)


revision : Jwt -> Revision.Id -> (Result Error ( Revision.Id, Revision ) -> msg) -> Cmd msg
revision token revisionId callback =
    let
        arguments =
            { projectId = Uuid revisionId.projectId
            , revisionNumber = revisionId.revisionNumber
            }

        query =
            ApiQuery.selection (,)
                |> with (ApiQuery.revision arguments idQuery)
                |> with (ApiQuery.revision arguments revisionQuery)

        idQuery =
            ApiRevision.selection Revision.Id
                |> with (uuidField ApiRevision.projectId)
                |> with ApiRevision.revisionNumber

        revisionQuery =
            ApiRevision.selection Revision
                |> with ApiRevision.htmlCode
                |> with ApiRevision.elmCode
                |> with (ApiRevision.packages packageQuery)
                |> with (Field.map (Maybe.withDefault "") ApiRevision.title)
                |> with (versionField ApiRevision.elmVersion)
                |> with (Field.map (Maybe.withDefault 1 >> TermsVersion.fromInt >> Result.withDefault TermsVersion.zero) ApiRevision.termsVersion)

        packageQuery =
            ApiPackage.selection (,)
                |> with (nameField ApiPackage.name)
                |> with (versionField ApiPackage.version)
    in
    query
        |> Graphqelm.queryRequest "/api"
        |> Jwt.withTokenHeader token
        |> Graphqelm.send (Result.mapError Graphqelm.ignoreParsedErrorData >> callback)
