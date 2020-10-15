module Merge.Diff where

import           Merge.ChangeDetection
import           Types

import qualified Data.Map              as M
import qualified Data.Set              as S

data DiffResult = DiffResult
  { notChanged :: Int
  }

diffChanges :: Changes -> Changes -> DiffResult
diffChanges change1 change2 = DiffResult 42
