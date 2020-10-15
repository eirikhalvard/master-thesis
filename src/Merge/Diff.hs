module Merge.Diff where

import           Merge.ChangeDetection
import           Types

import qualified Data.Map              as M
import qualified Data.Set              as S

data DiffResult = DiffResult
  { featureDiffResult :: M.Map FeatureId FeatureDiffResult
  , groupDiffResult   :: M.Map GroupId GroupDiffResult
  }

data FeatureDiffResult = FeatureDiffResult
  { unchanged     :: FeatureChange
  , newChange     :: FeatureChange
  , removedChange :: FeatureChange
  , changedChange :: (FeatureChange, FeatureChange)
  }

data GroupDiffResult = GroupDiffResult
  { gunchanged     :: GroupChange
  , gnewChange     :: GroupChange
  , gremovedChange :: GroupChange
  , gchangedChange :: (GroupChange, GroupChange)
  }

diffChanges :: Changes -> Changes -> DiffResult
diffChanges change1 change2 = undefined
