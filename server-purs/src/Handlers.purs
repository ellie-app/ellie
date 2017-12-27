module Handlers where

import Run (Run)
import Run.FileSystem (FILESYSTEM)
import Run.FileSystem as FileSystem
import Run.Aws (AWS)
import Run.Aws as Aws

type HandlerRun r = (fileSystem :: FILESYSTEM, aws :: AWS | r)

getRevision :: ∀ r. String -> Int -> Run (HandlerRun r) { id :: Int }