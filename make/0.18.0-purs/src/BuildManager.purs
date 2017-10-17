module BuildManager where

import Prelude
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.State as State
import Data.DateTime.Instant
import Elm.Compiler as Compiler
import Elm.Compiler.Error as Error
import Elm.Compiler.Module as Module
import Elm.Package as Package
import Elm.Package.Paths as Path
import System.FilePath (FilePath, (</>))
import TheMasterPlan as TMP
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple.Nested as Tuple
import Data.Either
import Data.Maybe
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Monad.Eff.Now as Now
import Control.Monad.Eff.Now (NOW)
import Data.Array ((:))
import Data.Array as Array


-- CONFIGURATION


newtype Config = Config
    { artifactDirectory :: FilePath
    , files :: Array FilePath
    }


outputFilePath :: FilePath
outputFilePath =
  "/build.js"


artifactDirectory :: FilePath
artifactDirectory =
    Path.stuffDirectory </> "build-artifacts" </> (show Compiler.version)

--
-- -- RUN A BUILD
--
--

type Task a =
  forall e. ExceptT Error (StateT (Array Phase) (Aff (now :: NOW | e))) a



run :: forall a e. Task a -> Aff (now :: NOW | e) (Either Error (a /\ Timeline))
run task = do
  result <- runStateT (runExceptT (phase "elm-make" task)) []
  case result of
    Right answer /\ [ Phase { tag, start, subphases, end } ] ->
      pure $ Right (answer /\ Timeline { start, phases: subphases, end })

    Left err /\ _ ->
      pure $ Left err

    Right _ /\ _ ->
      pure $ Left $ ImpossibleError "Something impossible happened when profiling elm-make."


-- TIMELINE


newtype Timeline = Timeline
    { start :: Instant
    , phases :: Array Phase
    , end :: Instant
    }


newtype Phase = Phase
    { tag :: String
    , start :: Instant
    , subphases :: Array Phase
    , end :: Instant
    }


phase :: forall a. String -> Task a -> Task a
phase name task = do
  phasesSoFar <- State.get
  State.put []
  start <- liftAff $ liftEff Now.now
  result <- task
  end <- liftAff $ liftEff Now.now
  State.modify (\phases -> Phase { tag: name, start, subphases: (Array.reverse phases), end } : phasesSoFar)
  pure result
--
--
-- timelineToString :: Timeline -> String
-- timelineToString (Timeline start phases end) =
--   let
--     duration = end - start
--   in
--     "\nOverall time: " ++ show duration ++ "\n"
--     ++ concatMap (phaseToString duration 1) phases
--     ++ "\n"
--
--
-- phaseToString :: Time.Instant -> Int -> Phase -> String
-- phaseToString overallDuration indent (Phase tag start subphases end) =
--   let
--     duration = end - start
--     percent = truncate (100 * duration / overallDuration) :: Int
--   in
--     '\n' : replicate (indent * 4) ' ' ++ show percent ++ "% - " ++ tag
--     ++ concatMap (phaseToString duration (indent + 1)) subphases
--
--
--
-- -- ERRORS
--
--
data Error
    = CompilerErrors FilePath String (Array Error.Error)
    | CorruptedArtifact FilePath
    | Cycle (Array TMP.CanonicalModule)
    | PackageProblem String
    | MissingPackage Package.Name
    | ModuleNotFound Module.Raw (Maybe Module.Raw)
    | ModuleDuplicates
        { name :: Module.Raw
        , parent :: Maybe Module.Raw
        , local :: Array FilePath
        , foreign :: Array Package.Name
        }
    | ModuleName
        { path :: FilePath
        , expectedName :: Module.Raw
        , actualName :: Module.Raw
        }
    | UnpublishablePorts FilePath Module.Raw
    | UnpublishableEffects FilePath Module.Raw
    | ImpossibleError String
