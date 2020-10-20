module Merge.Diff where

import           Merge.ChangeDetection
import           Merge.MergeTypes
import           Types

import qualified Data.Map              as M
import qualified Data.Set              as S

diffModifications :: Modifications -> Modifications -> DiffResult
diffModifications change1 change2 = DiffResult M.empty M.empty
