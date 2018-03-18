module Ellie.Api where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Entity (Entity)
import Data.Entity as Entity
import Data.Json as Json
import Data.Maybe (Maybe(..))
import Data.String (Pattern(Pattern))
import Data.String (stripPrefix) as String
import Data.TemplateString.Unsafe (template) as String
import Data.Url as Url
import Ellie.Domain.Assets (class Assets)
import Ellie.Domain.Assets as Assets
import Ellie.Domain.Platform (class Platform)
import Ellie.Domain.Platform as Platform
import Ellie.Domain.RevisionRepo (class RevisionRepo)
import Ellie.Domain.RevisionRepo as RevisionRepo
import Ellie.Domain.Search (class Search)
import Ellie.Domain.Search as Search
import Ellie.Domain.UserRepo (class UserRepo)
import Ellie.Domain.UserRepo as UserRepo
import Ellie.Types.Revision as Revision
import Ellie.Types.Settings as Settings
import Ellie.Types.TermsVersion as TermsVersion
import Ellie.Types.User (User(..))
import Ellie.Types.User as User
import Elm.Package.Searchable as Searchable
import Elm.Package as Package
import Server.Action (ActionT)
import Server.Action as Action
import System.Jwt (Jwt(..))


authorize ∷ ∀ m. Monad m ⇒ UserRepo m ⇒ ActionT m (Maybe (Entity User.Id User))
authorize = runMaybeT do
  tokenHeader ← MaybeT $ Action.getHeader "Authorization"
  token ← MaybeT $ pure $ String.stripPrefix (Pattern "Bearer ") tokenHeader
  userId ← MaybeT $ lift $ UserRepo.verify (Jwt token)
  MaybeT $ lift $ UserRepo.retrieve userId


authorizeParam ∷ ∀ m. Monad m ⇒ UserRepo m ⇒ ActionT m (Maybe (Entity User.Id User))
authorizeParam = runMaybeT $ do
  token ← MaybeT $ Action.getParam "token"
  userId ← MaybeT $ lift $ UserRepo.verify (Jwt token)
  MaybeT $ lift $ UserRepo.retrieve userId


getRevision ∷ ∀ m. Monad m ⇒ RevisionRepo m ⇒ ActionT m Unit
getRevision = do
  projectId ← Action.getParam "projectId"
  revisionNumber ← Action.getParam "revisionNumber"
  let
    maybeRevisionId =
      { projectId: _, revisionNumber: _ }
        <$> projectId
        <*> revisionNumber
        <#> Revision.Id
  case maybeRevisionId of
    Just revisionId → do
      maybeRevision ← lift $ RevisionRepo.retrieve revisionId
      case maybeRevision of
        Just entity → do
          Action.setStatus 200
          Action.setStringBody
            $ Json.stringify
            $ Revision.entityToBody
            $ entity
        Nothing →
          Action.setStatus 404
    Nothing →
      Action.setStatus 400


searchPackages ∷ ∀ m. Monad m ⇒ Search m ⇒ ActionT m Unit
searchPackages = do
  maybeQuery ← Action.getParam "query"
  case maybeQuery of
    Just query → do
      searchables ← lift $ Search.search query
      let packages = map Searchable.latestPackage searchables
      Action.setStatus 200
      Action.setStringBody
        $ Json.stringify
        $ Json.encodeArray Package.toBody packages
    Nothing →
      Action.setStatus 400


verify ∷ ∀ m. Monad m ⇒ UserRepo m ⇒ ActionT m Unit
verify = do
  user ← 
    authorize >>= case _ of
      Just user → pure user
      Nothing → lift $ UserRepo.create
  (Jwt token) ←
    lift $ UserRepo.sign (Entity.key user)
  Action.setStatus 201
  Action.setStringBody
    $ Json.stringify
    $ Json.encodeObject
        [ { key: "token", value: Json.encodeString token }
        , { key: "user", value: User.entityToBody user }
        ]


acceptTerms ∷ ∀ m. Monad m ⇒ UserRepo m ⇒ ActionT m Unit
acceptTerms = do
  authorize >>= case _ of
    Nothing →
      Action.setStatus 401
    Just entity → do
      termsVersionOrError ← do
        body ← Action.getBody
        pure $ Json.decodeAtField "termsVersion" body TermsVersion.fromJson
      case termsVersionOrError of
        Left error →
          Action.setStatus 400
        Right termsVersion → do
          let (User user) = Entity.record entity
          let userId = Entity.key entity
          let updated = User $ user { termsVersion = Just termsVersion }
          lift $ UserRepo.save userId updated
          Action.setStatus 204


saveSettings ∷ ∀ m. Monad m ⇒ UserRepo m ⇒ ActionT m Unit
saveSettings = do
  authorize >>= case _ of
    Nothing →
      Action.setStatus 401
    Just entity → do
      let userId = Entity.key entity
      let (User user) = Entity.record entity
      body ← Action.getBody
      let eitherSettings = Settings.fromBody body
      case eitherSettings of
        Left errors → Action.setStatus 400
        Right settings → do
          let newUser = User $ user { settings = settings }
          lift $ UserRepo.save userId newUser
          Action.setStatus 204


formatCode ∷ ∀ m. Monad m ⇒ Platform m ⇒ ActionT m Unit
formatCode = do
  body ← Action.getBody
  let code = Json.decodeAtField "code" body Json.decodeString
  case code of
    Left errors →
      Action.setStatus 400
    Right code → do
      result ← lift $ Platform.format code
      case result of
        Left message → do
          Action.setStatus 400
          Action.setHeader "Content-Type" "application/json"
          Action.setStringBody
            $ Json.stringify
            $ Json.encodeObject [ { key: "error", value: Json.encodeString message } ]
        Right code → do
          Action.setStatus 200
          Action.setHeader "Content-Type" "application/json"
          Action.setStringBody
            $ Json.stringify
            $ Json.encodeObject [ { key: "code", value: Json.encodeString code } ]


result ∷ ∀ m. Monad m ⇒ UserRepo m ⇒ Platform m ⇒ ActionT m Unit
result = do
  maybeUser ← authorizeParam 
  case maybeUser of
    Nothing → Action.setStatus 401
    Just entity → do
      { javascript, html } ← lift $ Platform.result $ Entity.key entity
      format ← Action.getParam "format"
      case format of
        Just "javascript" → Action.setFileBody javascript
        Just "html" → Action.setFileBody html
        _ → Action.setStatus 404


newUi ∷ ∀ m. Monad m ⇒ Assets m ⇒ UserRepo m ⇒ ActionT m Unit
newUi = do
  Action.setStatus 200
  (lift htmlContent) >>= Action.setStringBody
  where
    htmlTemplate ∷ String
    htmlTemplate =
        """
        <!DOCTYPE html>
        <html>
          <head>
            <meta charset="utf-8" />
            <base href="/" />
            <link rel="icon" href="${favicon}" />
            <title>Ellie - The Elm Live Editor</title>
            <meta property="og:image" content="${logo}" itemprop="thumbnailUrl" />
            <meta property="og:title" content="Ellie - The Elm Live Editor" />
            <meta property="og:url" content="https://ellie-app.com/new" />
            <meta property="og:site_name" content="Ellie" />
            <meta property="og:description" content="Ellie puts the Elm platform in your browser so you can try Elm, share ideas, and bring your programs to life." />
            <meta name="twitter:card" content="summary" />
            <meta name="twitter:url" content="https://ellie-app.com/new" />
            <meta name="twitter:title" content="Ellie - The Elm Live Editor" />
            <meta name="twitter:description" content="Ellie puts the Elm platform in your browser so you can try Elm, share ideas, and bring your programs to life." />
            <meta name="twitter:image" content="${logo}" />
            <meta name="latest_terms_version" content="1" />
            <meta name="accepted_terms_version" content="1" />
            <script src="https://cdnjs.cloudflare.com/ajax/libs/custom-elements/1.0.8/custom-elements.min.js"></script>
            <script async src="${javascript}"></script>
          </head>
          <body>
          </body>
        </html>
        """

    htmlContent ∷ m String
    htmlContent = do
      parameters ←
        { javascript: _, latestTermsVersion: _, favicon: _, logo: _ }
          <$> (Url.href <$> Assets.assetUrl "editor.js")
          <*> pure (show TermsVersion.latest)
          <*> (Url.href <$> Assets.assetUrl "images/favicon.ico")
          <*> (Url.href <$> Assets.assetUrl "images/logo.png")
      pure $ String.template htmlTemplate parameters
