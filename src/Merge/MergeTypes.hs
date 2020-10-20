module Merge.MergeTypes where

import           Types

import qualified Data.Map as M
import qualified Data.Set as S



--- Change between featuremodels ---

data Changes = Changes
  { featureChanges :: M.Map FeatureId FeatureChange
  , groupChanges   :: M.Map GroupId GroupChange
  }


data FeatureChange
  = FeatureAdd
  | FeatureRemove
  | FeatureChange
      (Maybe FeatureParentChange)
      (Maybe FeatureNameChange)
      (Maybe FeatureTypeChange)


data FeatureParentChange = FeatureParentChange GroupId


data FeatureNameChange = FeatureNameChange String


data FeatureTypeChange = FeatureTypeChange FeatureType


data GroupChange
  = GroupAdd
  | GroupRemove
  | GroupChange
      (Maybe GroupParentChange)
      (Maybe GroupTypeChange)


data GroupParentChange = GroupParentChange FeatureId


data GroupTypeChange = GroupTypeChange GroupType



--- Diff between base, v1, v2 ---

-- The diff result from the all the changes in the entire time point for all
-- versions of the model
data DiffResult = DiffResult
  { featureDiffResult :: M.Map FeatureId FeatureDiffResult
  , groupDiffResult   :: M.Map GroupId GroupDiffResult
  }


-- Every possible combination that a feature- or group change could be modified
data SingleDiffResult changeType
  = NoChange changeType
  | ChangedInOne Version (OneChange changeType)
  | ChangedInBoth (BothChange changeType)


data OneChange changeType
  = OneChangeWithBase
      changeType -- Base modification
      (RemovedOrChangedModification changeType) -- Derived (V1 or V2) modification
  | OneChangeWithoutBase
      (AddedModification changeType) -- Derived (V1 or V2) modification


data BothChange changeType
  = BothChangeWithBase
      changeType -- Base modification
      (RemovedOrChangedModification changeType) -- V1 modification
      (RemovedOrChangedModification changeType) -- V2 modification
  | BothChangeWithoutBase
      (AddedModification changeType) -- V1 modification
      (AddedModification changeType) -- V2 modification


data RemovedOrChangedModification changeType
  = RemovedModification
  | ChangedModification changeType


data AddedModification changeType
  = AddedModification changeType


data Version
  = V1
  | V2


type FeatureDiffResult = SingleDiffResult FeatureChange


type GroupDiffResult = SingleDiffResult GroupChange
