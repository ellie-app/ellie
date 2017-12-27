module Run.Aws where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Aff.Aws (Client, ClientOptions, CreatePresignedPostOptions, GetObjectOptions, PutObjectOptions)
import Control.Monad.Aff.Aws as AwsAff
import Data.Either as Either
import Data.StrMap (StrMap)
import Run (FProxy, Run, SProxy(..))
import Run as Run


data Aws a
  = CreateClient ClientOptions (Client -> a)
  | GetObject Client GetObjectOptions (String -> a)
  | PutObject Client PutObjectOptions a
  | GetObjectExists Client GetObjectOptions (Boolean -> a)
  | CreatePresignedPost Client CreatePresignedPostOptions (StrMap String -> a)
derive instance functorAws :: Functor Aws


type AWS = FProxy Aws


_aws = SProxy :: SProxy "aws"


createClient :: ∀ r. ClientOptions -> Run (aws :: AWS | r) Client
createClient options = Run.lift _aws $ CreateClient options id


getObject :: ∀ r. Client -> GetObjectOptions -> Run (aws :: AWS | r) String
getObject client options = Run.lift _aws $ GetObject client options id


putObject :: ∀ r. Client -> PutObjectOptions -> Run (aws :: AWS | r) Unit
putObject client options = Run.lift _aws $ PutObject client options unit


getObjectExists :: ∀ r. Client -> GetObjectOptions -> Run (aws :: AWS | r) Boolean
getObjectExists client options = Run.lift _aws $ GetObjectExists client options id


createPresignedPost :: ∀ r. Client -> CreatePresignedPostOptions -> Run (aws :: AWS | r) (StrMap String)
createPresignedPost client options = Run.lift _aws $ CreatePresignedPost client options id


interpretAff :: ∀ e. Aws ~> Aff (aws :: AwsAff.AWS | e)
interpretAff command =
  case command of
    CreateClient options next -> next <$> AwsAff.createClient options
    GetObject client options next -> next <$> AwsAff.getObject client options
    PutObject client options next -> next <$ AwsAff.putObject client options
    CreatePresignedPost client options next -> next <$> AwsAff.createPresignedPost client options
    GetObjectExists client options next ->
      AwsAff.headObject client options
        # Aff.attempt
        # map (Either.isRight >>> next)