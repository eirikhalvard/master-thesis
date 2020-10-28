module Merge.Diff where

import           Merge.ChangeDetection
import           Merge.MergeTypes
import           Types

import qualified Data.Map              as M
import qualified Data.Set              as S

-- diffModifications will compare the modifcations from base with the
-- modifications from each derived version. The comparison will produce
-- a DiffResult that represents how every feature- and group modification was
-- changed between the base and derived versions
diffModifications :: Modifications -> Modifications -> Modifications -> DiffResult
diffModifications baseModifications v1Modifications v2Modifications =
  DiffResult M.empty M.empty
