module Merge.Diff where

import           Merge.ChangeDetection
import           Types

import qualified Data.Map              as M
import qualified Data.Set              as S

data DiffResult = DiffResult
  { featureDiffResult :: M.Map FeatureId FeatureDiffResult
  , groupDiffResult   :: M.Map GroupId GroupDiffResult
  }

data SingleDiffResult change
  = Unchanged change
  | NewChange change
  | RemovedChange change
  | ChangedChange change change

type FeatureDiffResult = SingleDiffResult FeatureChange

type GroupDiffResult = SingleDiffResult GroupChange


data BothResult change
  = UUnchanged change
  | OnlyV1Change change change
  | OnlyV2Change change change
  | BothChange change change change





diffChanges :: Changes -> Changes -> DiffResult
diffChanges change1 change2 = DiffResult M.empty M.empty
